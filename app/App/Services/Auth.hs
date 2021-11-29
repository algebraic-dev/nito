module App.Services.Auth where

import Polysemy (Members, Sem)
import App.Services.Auth.JWT ( Sign, JWTData, encodeJWT )
import App.Services.Auth.Hashing ( validateHash, hashPass )
import App.Models.User ( UserStore, findUserByNickname, User (password, userID), insertNewUser )
import Data.Text (Text)
import Crypto.BCrypt (hashPassword)
import Data.UUID (toText)
import Data.Morpheus.Types (GQLType)
import GHC.Generics (Generic)
import Polysemy.Embed (Embed, embed)

newtype AuthError = AuthError { errorMessage :: Text }
    deriving (Generic, GQLType)

data LoginArgs = LoginArgs
    { loginUsername :: Text
    , loginPassword :: Text
    } deriving (Generic, GQLType)

data LoginResponse = LoginResponse
    { loginID :: Maybe Text
    , loginError :: Maybe AuthError
    } deriving (Generic, GQLType)

data RegisterArgs = RegisterArgs
    { registerUsername :: Text
    , registerPassword :: Text
    } deriving (Generic, GQLType)

data RegisterResponse = RegisterResponse
    { registerID :: Maybe Text
    , registerError :: Maybe AuthError
    } deriving (Generic, GQLType)

runLogin :: Members [Sign, UserStore] r => LoginArgs -> Sem r LoginResponse
runLogin (LoginArgs name pass) = do
    user <- findUserByNickname name
    case user of
      Nothing -> pure (errored "Invalid credentials")
      Just user | validateHash (password user) pass ->
          succeded <$> encodeJWT (toText $ userID user)
      Just user ->
          pure (errored "Invalid credentials")
    where
        errored :: Text -> LoginResponse
        errored         = LoginResponse Nothing . Just . AuthError
        succeded userID = LoginResponse (Just userID) Nothing

runRegister :: Members [Sign, UserStore, Embed IO] r => RegisterArgs -> Sem r RegisterResponse
runRegister (RegisterArgs name pass) = do
    user <- findUserByNickname name
    case user of
      Nothing -> do
        hashed <- embed $ hashPass pass
        user   <- insertNewUser name hashed
        pure $ maybe (errored "Error while trying to create your account!")
                     succeded
                     user
      Just user -> pure (errored "There's an account with")
    where
        errored         = RegisterResponse Nothing . Just . AuthError
        succeded userID = RegisterResponse (Just userID) Nothing
    