module Extra.TH where

import Data.Aeson.TH
import Language.Haskell.TH (Name)
import Language.Haskell.TH.Syntax (Q, Dec)

myDeriveJson :: Name -> Q [Dec]
myDeriveJson = deriveJSON defaultOptions
