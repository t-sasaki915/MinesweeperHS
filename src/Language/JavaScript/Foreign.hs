module Language.JavaScript.Foreign
    ( consoleLog_
    , createElement_
    , setElementId_
    , setElementClassName_
    , appendChildToBody_
    ) where

import           GHC.JS.Prim

foreign import javascript "((message) => console.log(message))"
    consoleLog_ :: JSVal -> IO ()

foreign import javascript "((elementName) => document.createElement(elementName))"
    createElement_ :: JSVal -> IO JSVal

foreign import javascript "((element, newId) => element.id = newId)"
    setElementId_ :: JSVal -> JSVal -> IO ()

foreign import javascript "((element, newClassName) => element.className = newClassName)"
    setElementClassName_ :: JSVal -> JSVal -> IO ()

foreign import javascript "((element) => document.body.appendChild(element))"
    appendChildToBody_ :: JSVal -> IO ()
