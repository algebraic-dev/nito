module App.Resolvers.User where 
import Data.Morpheus.Types (GQLType, ResolverQ)
import GHC.Generics (Generic)

import App.Models.User

data LoginArgs = LoginArgs 
    { loginUsername :: String
    , loginPassword :: String
    }

data RegisterArgs = RegisterArgs 
    { registerUsername :: String
    , registerPassword :: String
    }

resolveDeity :: LoginArgs -> ResolverQ () IO User
resolveDeity args = undefined