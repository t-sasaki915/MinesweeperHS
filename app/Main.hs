{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import           Control.Lens                  (makeLenses, over)
import           Data.Aeson                    (FromJSON (..), ToJSON (..),
                                                Value (..), object, (.:), (.=))
import           Language.JavaScript.Framework
import           Language.JavaScript.Wrapper

data GameState = GameState
    { _counter :: Int
    } deriving Show

instance ToJSON GameState where
    toJSON (GameState counter) = object
        [ "counter" .= counter
        ]

instance FromJSON GameState where
    parseJSON (Object v) = GameState <$> (v .: "counter")

    parseJSON _          = fail "Failed to parse GameState."

instance AppState GameState

makeLenses ''GameState

main :: IO ()
main = do
    initialiseAppState (GameState { _counter = 0 })

    gameContainer <- getElementById "gameContainer"

    element <- createElement Div
    setElementId "asdf" element
    setElementClassName "gameCell closedCell" element

    appendChild gameContainer element

    gameState <- getAppState :: IO GameState
    print gameState

    setAppState (over counter (+ 1) gameState)

    gameState2 <- getAppState :: IO GameState
    print gameState2

    consoleLog "TEST"
