module App.Models.User where 

import Data.Text (Text)
import Polysemy (Member, Embed, Sem, interpret, embed, makeSem)
import Database.PostgreSQL.Simple ( Connection, query_, SqlError, query, Only (Only) )
import Database.PostgreSQL.Simple.FromRow (FromRow (fromRow), field)
import Data.UUID ( UUID )
import Control.Exception (catch)
import Data.Maybe (listToMaybe)

data User = User 
    { id       :: UUID 
    , username :: String
    , password :: String 
    } deriving (Show)

data UserStore m a where
  FindUserById       :: Text -> UserStore m (Maybe User)
  FindUserByNickname :: Text -> UserStore m (Maybe User)

-- makeSem ''UserStore
-- Postgres adapter for the Store 

instance FromRow User where 
    fromRow = User <$> field <*> field <*> field
    
catchExp :: IO (Maybe User) -> IO (Maybe User)
catchExp ext = catch ext (const $ pure Nothing ::  SqlError -> IO (Maybe User))

runPostgresUserStore ::  Member (Embed IO) r => Connection -> Sem (UserStore ': r) a -> Sem r a
runPostgresUserStore conn = interpret \case
  FindUserById id       -> embed $ treatQuery $ query conn "select * from users where id= ?" (Only id)
  FindUserByNickname id -> embed $ treatQuery $ query conn "select * from users where nickname=?" (Only id) 
  where treatQuery = catchExp . (listToMaybe <$>)
    