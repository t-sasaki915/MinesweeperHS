module Language.JavaScript.Wrapper.ElementWrapper
    ( Element (..)
    , ElementType (..)
    , elementTypeName
    ) where

import           GHC.JS.Prim (JSVal, toJSString)

newtype Element = Element JSVal

data ElementType = Div
                 deriving (Show, Eq)

elementTypeName :: ElementType -> JSVal
elementTypeName Div = toJSString "div"
