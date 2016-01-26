module Main where

import Prelude
import Control.Monad.Eff.Console
import qualified Control.Monad.Eff.JQuery as JQuery
import Network.HTTP.Affjax
import Network.HTTP.Method
import Control.Monad.Aff
import Control.Monad.Eff
import Control.Monad.Eff.Class

foreign import rtmStartUrl :: String
foreign import currentChannel :: String

main = JQuery.ready $ do
  launchAff $ do
    res <- affjax $ defaultRequest { url = rtmStartUrl, method = GET }
    liftEff $ log $ "GET /api response: " ++ res.response
  return unit