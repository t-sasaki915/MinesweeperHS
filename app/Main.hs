{-# LANGUAGE CPP #-}

module Main (main) where

import           GHC.JS.Prim

foreign import javascript "((message) => console.log(message))"
    consoleLog :: JSVal -> IO ()

main :: IO ()
main = consoleLog (toJSString "TEST")
