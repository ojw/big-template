{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module BigTemplate.Types where

import qualified Data.Text as T
import Data.Aeson
import GHC.Generics
import Control.Applicative (empty)

type UserId = Int

data User = User
  { userId :: UserId
  , userName :: T.Text
  , userEmail :: T.Text
  } deriving (Show, Generic)

data UserRegistration = UserRegistration
  { registrationName :: T.Text
  , registrationEmail :: T.Text
  } deriving Generic

instance FromJSON User where
  parseJSON (Object v) = User <$>
                         v .: "userId" <*>
                         v .: "userName" <*>
                         v .: "userEmail"
  parseJSON _ = empty

instance ToJSON User where
  toJSON (User userId userName userEmail) =
    object [ "userId" .= userId
           , "userName" .= userName
           , "userEmail" .= userEmail
           ]

-- instance FromJSON User
-- instance ToJSON User
instance FromJSON UserRegistration
instance ToJSON UserRegistration
