module App.Config (loadDotEnv, Env(..), readDbConfig, readConfig, AppConfig (..)) where

import Configuration.Dotenv (Config (configPath), defaultConfig, loadFile)
import Configuration.Dotenv.Environment (lookupEnv)
import Control.Monad (void)
import Data.Word (Word16)

import Database.PostgreSQL.Simple
  ( ConnectInfo (ConnectInfo),
    Connection,
    Only (Only),
    connect,
    query_,
  )

import Polysemy (Embed, Members, Sem, embed)
import Polysemy.Fail (Fail)
import Polysemy.Reader (Reader, asks)
import System.Environment (getEnv)
import Data.Text (Text, pack)
import Web.JWT (hmacSecret, Signer)

data AppConfig = AppConfig { appDbInfo :: ConnectInfo
                           , appSigner :: Signer }

data Env = Env { envConn :: Connection
               , envInfo :: AppConfig}

readDbConfig :: IO ConnectInfo
readDbConfig =
  ConnectInfo
    <$> getEnv "DB_HOST"
    <*> (read <$> getEnv "DB_PORT")
    <*> getEnv "DB_USER"
    <*> getEnv "DB_PASS"
    <*> getEnv "DB_NAME"

readConfig :: IO AppConfig
readConfig = AppConfig
                <$> readDbConfig
                <*> (hmacSecret . pack <$> getEnv "SECRET")

loadDotEnv :: IO ()
loadDotEnv = do
  production <- lookupEnv "ENV"
  case production of
    Just "PROD" -> pure ()
    _ -> void $ loadFile (defaultConfig {configPath = [".env"]})