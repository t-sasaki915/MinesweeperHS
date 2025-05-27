module Language.JavaScript.Wrapper.Internal.Foreign
    ( consoleLog_
    , createElement_
    , setElementId_
    , setElementClassName_
    , appendChildToBody_
    ) where

import           GHC.JS.Prim (JSVal)

foreign import javascript "((message) => console.log(message))"
    consoleLog_ :: JSVal -> IO ()

foreign import javascript "((elementTypeName) => document.createElement(elementTypeName))"
    createElement_ :: JSVal -> IO JSVal

foreign import javascript "((newId, element) => element.id = newId)"
    setElementId_ :: JSVal -> JSVal -> IO ()

foreign import javascript "((newClassName, element) => element.className = newClassName)"
    setElementClassName_ :: JSVal -> JSVal -> IO ()

foreign import javascript "((element) => document.body.appendChild(element))"
    appendChildToBody_ :: JSVal -> IO ()
