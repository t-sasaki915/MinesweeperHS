module Language.JavaScript.Wrapper
    ( Element (..)
    , ElementType (..)
    , EventType (..)
    , createElement
    , setElementId
    , setElementClassName
    , appendChild
    , getElementById
    , addEventListener
    , addEventListenerWithState
    ) where

import           Language.JavaScript.Wrapper.ElementWrapper
import           Language.JavaScript.Wrapper.EventWrapper
import           Language.JavaScript.Wrapper.WrappedForeignFunction
