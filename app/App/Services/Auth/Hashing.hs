module App.Services.Auth.Hashing (hashPass, validateHash) where

import Crypto.BCrypt
    ( hashPasswordUsingPolicy,
      slowerBcryptHashingPolicy,
      validatePassword )

import Data.ByteString.Char8 (pack)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Data.Function (on)
import Data.Functor ((<&>))

hashPass :: Text -> IO Text
hashPass text = do 
    res <- hashPasswordUsingPolicy slowerBcryptHashingPolicy (encodeUtf8 text) 
        <&> (decodeUtf8 <$>)
    case res of 
      Nothing  -> error "Hash function returned an unexpected Nothing"
      Just txt -> pure txt
        

validateHash :: Text -> Text -> Bool
validateHash = validatePassword `on` encodeUtf8