{-# LANGUAGE CPP #-}

module Main (main) where

main :: IO ()
main = putStrLn "AAA"

#ifdef WASM
foreign export javascript "hs_start" main :: IO ()
#endif
