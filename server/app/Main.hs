module Main where

import Network.Wai.Handler.Warp

import BigTemplate.Types
import BigTemplate.Routes
import BigTemplate.Server

main :: IO ()
main = do
  userState <- newUserState
  run 8080 (bigTemplateApp userState)
