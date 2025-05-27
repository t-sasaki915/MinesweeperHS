module Language.JavaScript.Wrapper.EventWrapper
    ( EventType (..)
    , eventTypeName
    ) where

import           GHC.JS.Prim (JSVal, toJSString)

data EventType = Click
               deriving Show

eventTypeName :: EventType -> JSVal
eventTypeName Click = toJSString "click"
