module Main where

import App.Config ( readConfig, loadDotEnv, AppConfig, Env (Env) )
import Polysemy.Fail (runFail)
import Polysemy.Reader (runReader)
import Polysemy (runM)
import App.Database (connectDB)
import Database.PostgreSQL.Simple (query, query_, Only (Only), Connection)
import App.Models.User (User, runPostgresUserStore)
import Web.JWT (hmacSecret)
import Web.Scotty ( post, scotty, body )
import Web.Scotty.Trans (raw, get)
import Control.Monad.Cont (MonadIO(liftIO))
import App.Resolvers.Root (gqlApi)
import Data.Morpheus.Server (httpPlayground)

main :: IO ()
main = do
        loadDotEnv
        config <- readConfig
        putStrLn "Waiting for database to connect..."
        Right conn <- connectDatabase config
        putStrLn "Connected!"
        scotty 3000 $ do
            get "/" (raw httpPlayground)
            post "/api" 
                $ raw
                =<< (liftIO . gqlApi (Env conn config) =<< body)
    where
        connectDatabase config = runM $ runFail (runReader config connectDB)