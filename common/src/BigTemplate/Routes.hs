{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module BigTemplate.Routes where

import Servant.API
import Data.Proxy

import BigTemplate.Types

type BigTemplateRoutes =
       "users" :> Get '[JSON] Int -- [User]
  :<|> "user" :> Capture "id" UserId :> Get '[JSON] User
  :<|> "register" :> ReqBody '[JSON] UserRegistration :> Post '[JSON] UserId

bigTemplateRoutes :: Proxy BigTemplateRoutes
bigTemplateRoutes = Proxy
