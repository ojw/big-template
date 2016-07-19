module Main where

import Reflex.Dom
import Servant.Reflex
import BigTemplate.Client

main :: IO ()
main =
  mainWidget runGUI
  -- mainWidget $ el "div" $ text "Ho buddy."
