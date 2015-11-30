{-# LANGUAGE RecursiveDo #-}

module Main (
    main
) where

-- import Control.Applicative ((<$>))
-- import GHCJS.DOM
--        (enableInspector, webViewGetDomDocument, runWebGUI)
-- import GHCJS.DOM.Document (getBody, createElement, createTextNode, click)
-- import GHCJS.DOM.Element (setInnerHTML)
-- import GHCJS.DOM.Node (appendChild)
-- import GHCJS.DOM.EventM (on, mouseClientXY)
import Reflex
import Reflex.Dom
import qualified Data.Map as Map
import Safe (readMay)


-- main :: IO ()
-- main = mainWidget $ el "div" $ do
--   rec _ <- dynText $ _textInput_value t
--       t <- textInput def
--   return ()

-- main :: IO ()
-- main = mainWidget $ el "div" $ do
--   rec _ <- dynText $ _textInput_value t
--       t <- el "div" $ do
--         textInput def
--   return ()

main :: IO ()
main = mainWidget $ el "div" $ do
  rec _ <- dynText $ _textInput_value t
      t <- el "div" $ do
        textInput def
  return ()

-- main :: IO ()
-- main = mainWidget $ el "div" $ do
--   nx <- numberInput
--   d <- dropdown "*" (constDyn ops) def
--   ny <- numberInput
--   values <- combineDyn (,) nx ny
--   result <- combineDyn (\o (x, y) -> stringToOp o <$> x <*> y) (_dropdown_value d) values
--   resultString <- mapDyn show result
--   text " = "
--   dynText resultString

-- numberInput :: MonadWidget t m => m (Dynamic t (Maybe Double))
-- numberInput = do
--   n <- textInput $ def & textInputConfig_inputType .~ "number"
--                        & textInputConfig_initialValue .~ "0"
--   mapDyn readMay $ _textInput_value n

-- numberInput :: MonadWidget t m => m (Dynamic t (Maybe Double))
-- numberInput = do
--   let attrs = constDyn $ Map.fromList [("style", "border-color: blue")]
--   n <- textInput $ def & textInputConfig_inputType .~ "number"
--                        & textInputConfig_initialValue .~ "0"
--                        & textInputConfig_attributes .~ attrs
--   mapDyn readMay $ _textInput_value n

-- numberInput :: (MonadWidget t m) => m (Dynamic t (Maybe Double))
-- numberInput = do
--   let errorState = Map.singleton "style" "border-color: red"
--       validState = Map.singleton "style" "border-color: blue"
--   rec n <- textInput $ def & textInputConfig_inputType .~ "number"
--                            & textInputConfig_initialValue .~ "0"
--                            & textInputConfig_attributes .~ attrs
--       result <- mapDyn readMay $ _textInput_value n
--       attrs <- mapDyn (\r -> case r of
--                                   Just _ -> validState
--                                   Nothing -> errorState) result
--   return result

-- ops = Map.fromList [ ("+", "+")
--                    , ("-", "-")
--                    , ("*", "*")
--                    , ("/", "/") ]

-- stringToOp s =
--   case s of
--     "-" -> (-)
--     "*" -> (*)
--     "/" -> (/)
--     _   -> (+)
