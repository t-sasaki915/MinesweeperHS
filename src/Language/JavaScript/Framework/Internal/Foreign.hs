module Language.JavaScript.Framework.Internal.Foreign
    ( getAppState_
    , setAppState_
    ) where

import           GHC.JS.Prim (JSVal)

foreign import javascript "(() => { return window.appState; })"
    getAppState_ :: IO JSVal

foreign import javascript "((newState) => { window.appState = newState; })"
    setAppState_ :: JSVal -> IO ()
