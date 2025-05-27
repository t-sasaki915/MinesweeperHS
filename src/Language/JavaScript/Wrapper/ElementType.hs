module Language.JavaScript.Wrapper.ElementType (ElementType (..), elementName) where

import           GHC.JS.Prim (JSVal, toJSString)

data ElementType = Div
                 deriving (Show, Eq)

elementName :: ElementType -> JSVal
elementName Div = toJSString "div"
