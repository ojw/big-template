{-# LANGUAGE DeriveGeneric #-}

module BigTemplate.Types where

import qualified Data.Text as T
import Data.Aeson
import GHC.Generics


type UserId = Int

data User = User
  { userId :: UserId
  , userName :: T.Text
  , userEmail :: T.Text
  } deriving Generic

data UserRegistration = UserRegistration
  { registrationName :: T.Text
  , registrationEmail :: T.Text
  } deriving Generic

instance FromJSON User
instance ToJSON User
instance FromJSON UserRegistration
instance ToJSON UserRegistration
