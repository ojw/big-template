{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module BigTemplate.Server where

import Servant
import qualified Data.IntMap as I
import Data.IORef
import qualified Data.Text as T
import Control.Monad.IO.Class
import Network.Wai

import BigTemplate.Types
import BigTemplate.Routes

data UserState = UserState
  { nextId :: UserId
  , users  :: I.IntMap User
  }

newUserState :: IO (IORef UserState)
newUserState = newIORef (UserState 0 I.empty)

registerUser' :: UserRegistration -> UserState -> (UserId, UserState)
registerUser' registration state = (next', UserState next' users')
  where
    next = nextId state
    user = User next (registrationName registration) (registrationEmail registration)
    next' = succ next
    users' = I.insert next user (users state)

registerUser :: MonadIO m => IORef UserState -> UserRegistration -> m UserId
registerUser userStateRef reg = do
  userState <- liftIO $ readIORef userStateRef
  let (userId, userState') = registerUser' reg userState
  liftIO $ writeIORef userStateRef userState'
  return userId

lookupUser' :: UserId -> UserState -> Maybe User
lookupUser' userId state = I.lookup userId (users state)

lookupUser :: IORef UserState -> UserId -> Handler User
lookupUser userStateRef userId = do
  userState <- liftIO $ readIORef userStateRef
  case lookupUser' userId userState of
    Nothing -> throwError err404
    Just userId -> return userId

getUsers' :: UserState -> [User]
getUsers' state = I.elems (users state)

getUsers :: MonadIO m => IORef UserState -> m Int
getUsers userStateRef = do
  -- userState <- liftIO $ readIORef userStateRef
  -- return $ getUsers' userState
  -- liftIO $ putStrLn "hit the route"
  -- return [User 1 (T.pack "James") (T.pack "email")]
  return 10

makeUserMap :: IO (IORef (I.IntMap User))
makeUserMap = newIORef I.empty

bigTemplateServer :: IORef UserState -> Server BigTemplateRoutes
bigTemplateServer userStateRef =
       getUsers userStateRef
  :<|> lookupUser userStateRef
  :<|> registerUser userStateRef

bigTemplateApp :: IORef UserState -> Application
bigTemplateApp ref = serve bigTemplateRoutes (bigTemplateServer ref)

type ApiWithFiles = "static" :> Raw :<|> BigTemplateRoutes

fullBigTemplateServer :: IORef UserState -> Server ApiWithFiles
fullBigTemplateServer ref = serveDirectory "/home/james/code/big-template/client/.stack-work/dist/x86_64-linux/Cabal-1.24.0.0_ghcjs/build/big-template-exe/big-template-exe.jsexe" :<|> bigTemplateServer ref

fullBigTemplateApp :: IORef UserState -> Application
fullBigTemplateApp ref = serve (Proxy :: Proxy ApiWithFiles) (fullBigTemplateServer ref)
