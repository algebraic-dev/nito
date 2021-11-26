module App.Database where 

import Polysemy ( embed, Embed, Members, Sem )
import Polysemy.Fail (Fail)
import Polysemy.Reader (Reader, asks)
import App.Config (AppConfig (dbInfo))
import Database.PostgreSQL.Simple ( query_, connect, Only(Only), Connection )

connectDB :: (Members [Fail, Reader AppConfig, Embed IO] r) => Sem r Connection
connectDB = do
    config   <- asks dbInfo
    conn     <- embed (connect config)
    [Only 4] <- embed (query_ conn "select 2+2" :: IO [Only Int])
    pure conn
    
