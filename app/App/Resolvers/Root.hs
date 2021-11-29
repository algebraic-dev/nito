module App.Resolvers.Root where
import Data.Morpheus.Types (RootResolver (..), GQLType, Undefined (..))
import GHC.Generics (Generic)
import App.Services.Auth (LoginArgs, LoginResponse, RegisterArgs, RegisterResponse)
import Data.Text (Text)
import App.Resolvers.User (resolveLogin, resolveRegister)
import Data.ByteString.Lazy.Internal
import App.Config (Env(..))
import Polysemy (Members, Embed, Sem, runM, Member)
import Polysemy.Reader (Reader, runReader)
import Data.Morpheus (interpreter)
import Data.Typeable (Typeable)

data Query m = Query
  { login :: LoginArgs -> m LoginResponse
  , register :: RegisterArgs -> m RegisterResponse 
  } deriving (Generic, GQLType)


rootResolver ::  Members [Reader Env, Embed IO] r => RootResolver (Sem r) () Query Undefined Undefined
rootResolver =
  RootResolver
    { queryResolver = Query resolveLogin resolveRegister
    , mutationResolver = Undefined
    , subscriptionResolver = Undefined
    }

gqlApi :: Env -> ByteString -> IO ByteString
gqlApi env bs = let res = interpreter rootResolver bs 
                in runM $ runReader env res