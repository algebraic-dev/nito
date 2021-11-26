module Main where

import App.Config ( readConfig, loadDotEnv )
import Polysemy.Fail (runFail)
import Polysemy.Reader (runReader)
import Polysemy (runM)
import App.Database (connectDB)
import Database.PostgreSQL.Simple (query, query_, Only (Only))
import App.Models.User (User, runPostgresUserStore)
import App.Auth.JWT ( JWTData(JWTData), runJWTSigner )
import Web.JWT (hmacSecret)

main :: IO ()
main = do
        loadDotEnv
        config <- readConfig
        putStrLn "Waiting for database to connect..."
        Right conn <- connectDatabase 
        putStrLn "Connected!"
    where 
        connectDatabase = runM $ runFail $ runReader config connectDB
    
    