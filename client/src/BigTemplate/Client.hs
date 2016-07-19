{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
-- {-# LANGUAGE AllowAmbiguousTypes #-} -- doesn't help

module BigTemplate.Client where

import Servant.API
import Data.Proxy
import Servant.Reflex
-- import Reflex
import Reflex.Dom

import BigTemplate.Types
import BigTemplate.Routes

main' :: IO ()
main' = mainWidget runGUI

-- getUsers :: MonadWidget t m => Event t () -> m (Event t (ReqResult [User]))
-- lookupUser :: MonadWidget t m => Behavior t (Either String Int)
--            -> Event t () -> m (Event t (ReqResult User))
-- registerUser :: MonadWidget t m => Behavior t (Either String UserRegistration)
--              -> Event t () -> m (Event t (ReqResult Int))
-- (getUsers :<|> lookupUser :<|> registerUser) =
--         client
--         bigTemplateRoutes
--         (Proxy :: Proxy (m :: * -> *))
--         (constDyn (BasePath "/"))

-- runGUI :: MonadWidget t m => m ()
runGUI = do
  url <- baseUrlWidget
  text "hello"

  let (getUsers :<|> lookupUser :<|> registerUser) =
        client
        bigTemplateRoutes
        (Proxy :: Proxy (m :: * -> *))
        url

  usersBtn <- button "get users"
  users <- getUsers usersBtn

  -- r <- holdDyn "Waiting" $ fmap _xhrResponse_responseText users
  -- dynText r
                                                     -- (constDyn (BasePath "/"))
  return ()
