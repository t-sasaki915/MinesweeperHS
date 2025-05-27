module Language.JavaScript.Wrapper.Internal.Foreign
    ( createElement_
    , setElementId_
    , setElementClassName_
    , appendChild_
    , getElementById_
    , addEventListener_
    ) where

import           GHC.JS.Foreign.Callback (Callback)
import           GHC.JS.Prim             (JSVal)

foreign import javascript "((elementTypeName) => document.createElement(elementTypeName))"
    createElement_ :: JSVal -> IO JSVal

foreign import javascript "((newId, element) => element.id = newId)"
    setElementId_ :: JSVal -> JSVal -> IO ()

foreign import javascript "((newClassName, element) => element.className = newClassName)"
    setElementClassName_ :: JSVal -> JSVal -> IO ()

foreign import javascript "((parent, child) => parent.appendChild(child))"
    appendChild_ :: JSVal -> JSVal -> IO ()

foreign import javascript "((elementId) => document.getElementById(elementId))"
    getElementById_ :: JSVal -> IO JSVal

foreign import javascript "((eventType, listener, element) => element.addEventListener(eventType, listener))"
    addEventListener_ :: JSVal -> Callback (IO ()) -> JSVal -> IO ()
