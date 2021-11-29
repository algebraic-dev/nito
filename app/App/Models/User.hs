module App.Models.User 
  ( User (..)
  , runPostgresUserStore
  , insertNewUser
  , UserStore (..)
  , findUserById
  , findUserByNickname) where

import Control.Exception (catch)
import Data.Maybe (listToMaybe)
import Data.Text (Text, pack)
import Database.PostgreSQL.Simple (Connection, Only (Only), Query, SqlError, query, query_, execute, returning)
import Database.PostgreSQL.Simple.FromRow (FromRow (fromRow), field)
import GHC.Generics (Generic)
import Polysemy (Embed, Member, Sem, embed, interpret, makeSem)
import Data.Int ( Int64 )
import Data.Time (UTCTime)
import Data.UUID (UUID, toText)

data User = User
  { userID    :: UUID,
    username :: Text,
    password :: Text
  }
  deriving (Show, Generic, FromRow)

data UserStore m a where
  FindUserById :: Text -> UserStore m (Maybe User)
  FindUserByNickname :: Text -> UserStore m (Maybe User)
  InsertNewUser :: Text -> Text -> UserStore m (Maybe Text)

makeSem ''UserStore

data Res = Res Text
instance (FromRow Res) where fromRow = Res <$> field 
    
-- Postgres adapter for the Store
runPostgresUserStore :: Member (Embed IO) r => Connection -> Sem (UserStore ': r) a -> Sem r a
runPostgresUserStore conn = interpret \case
  FindUserById numId      -> embed $ treatQuery $ query conn queryById (Only numId)
  FindUserByNickname nick -> embed $ treatQuery $ query conn queryByNick (Only nick)
  InsertNewUser name pass -> embed $ catchExp $ 
    do let insertUser = "insert into users (username, password) values (?, ?) returning id"
       xs :: [Only UUID]  <- returning conn insertUser [(name, pass)]
       case xs of 
        []          -> pure Nothing
        (Only e) : _ -> pure (Just $ toText e)
  where
    treatQuery   = catchExp . (listToMaybe <$>)
    queryById    = "select * from users where id= ?" :: Query
    queryByNick  = "select * from users where nickname=?" :: Query
    catchExp ext = catch ext (const $ pure Nothing :: SqlError -> IO (Maybe a))
        