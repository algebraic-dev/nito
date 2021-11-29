{-# LANGUAGE ConstraintKinds #-}

module App.Resolvers.User where

import App.Models.User (runPostgresUserStore, UserStore)
import App.Services.Auth.JWT (runJWTSigner, Sign)
import App.Services.Auth (LoginArgs, runLogin, LoginResponse, RegisterArgs, runRegister, RegisterResponse)
import Data.Morpheus.Types (ResolverQ, lift)
import Polysemy (Sem, runM, Member, Members, embed, Embed)
import Polysemy.Reader (asks, ask, Reader, runReader)
import Data.Text (Text)
import App.Config (Env(..), AppConfig (appSigner))

type GQuery r a = ResolverQ () (Sem r) a
type Environment r = (Members [Reader Env, Embed IO] r)

resolveAuth :: Environment r => Sem (Sign : UserStore : r) b -> Sem r b
resolveAuth sem = 
  do conn   <- asks envConn
     signer <- asks (appSigner . envInfo)
     runPostgresUserStore conn $ runJWTSigner signer sem

resolveLogin :: Environment r => LoginArgs -> GQuery r LoginResponse 
resolveLogin args = lift $ resolveAuth (runLogin args)

resolveRegister :: Environment r => RegisterArgs -> GQuery r RegisterResponse
resolveRegister args = lift $ resolveAuth (runRegister args)