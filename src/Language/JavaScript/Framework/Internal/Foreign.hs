module Language.JavaScript.Framework.Internal.Foreign
    ( consoleLog_
    , getAppState_
    , setAppState_
    ) where

import           GHC.JS.Prim (JSVal)

foreign import javascript "((message) => console.log(message))"
    consoleLog_ :: JSVal -> IO ()

foreign import javascript "(() => { return window.appState; })"
    getAppState_ :: IO JSVal

foreign import javascript "((newState) => { window.appState = newState; })"
    setAppState_ :: JSVal -> IO ()
