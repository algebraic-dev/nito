module App.Services.Auth.JWT where

import Polysemy ( interpret, Sem, makeSem, Member, embed )
import Data.Text (Text)

import qualified Data.Map.Strict as Map

import Data.Aeson.Types ( Value(String, Object), FromJSON (parseJSON) )
import Data.Scientific ( Scientific(base10Exponent) )
import Data.Time.Clock ( UTCTime, getCurrentTime )
import Data.Time.Clock.POSIX ( getCurrentTime, utcTimeToPOSIXSeconds, posixDayLength )
import Data.Functor ((<&>))
import Polysemy.Embed (Embed)
import Data.Time (nominalDay, addUTCTime)
import Control.Monad (guard)

import Web.JWT
    ( claims,
      decode,
      encodeSigned,
      numericDate,
      verify,
      ClaimsMap(ClaimsMap, unClaimsMap),
      JWTClaimsSet(exp, unregisteredClaims),
      Signer )

newtype JWTData = JWTData { jwtUserID :: Text } deriving Show

data Sign m a where
  EncodeJWT :: Text -> Sign m Text
  DecodeJWT :: Text -> Sign m (Maybe JWTData)

makeSem ''Sign

runJWTSigner :: Member (Embed IO) r => Signer -> Sem (Sign ': r) a -> Sem r a
runJWTSigner signer = interpret \case
  (EncodeJWT info) -> do
      time        <- embed getCurrentTime
      let expTime  = posixDayLength * 30
      let map      = Map.fromList [("id", String info)]
      pure $ encodeSigned signer mempty
           $ mempty 
              { unregisteredClaims = ClaimsMap map
              , Web.JWT.exp = numericDate $ utcTimeToPOSIXSeconds (addUTCTime expTime time)
              }
  (DecodeJWT text) -> do
      time <- embed getCurrentTime
      pure $ decode text
         >>= verify signer
         >>= \jwt  -> Web.JWT.exp (claims jwt)
         >>= \exp  -> numericDate (utcTimeToPOSIXSeconds time)
         >>= guard . (< exp)
         >>  getID jwt
         >>= parseJSON
    where
      parseJSON (String n) = pure (JWTData n)
      parseJSON _          = Nothing
      getID                = (Map.!? "id")
                           . unClaimsMap
                           . unregisteredClaims
                           . claims