{-# LANGUAGE RankNTypes #-}

module BigTemplate.Client where

import Servant.API
import Data.Proxy
import Servant.Reflex
import Reflex
import Reflex.Dom

import BigTemplate.Types
import BigTemplate.Routes

someUser :: UserId
someUser = 1

runGUI :: MonadWidget t m => m ()
-- runGUI :: IO ()
runGUI = do
  url <- baseUrlWidget
  text "hello"

  let (getUsers :<|> lookupUser :<|> registerUser) =
        undefined -- client
        bigTemplateRoutes
        undefined -- (Proxy :: Proxy m)
        url
                                                       -- (constDyn (BasePath "/"))
  return ()
