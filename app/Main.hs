{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import           Control.Lens                  (makeLenses, over)
import           Data.Aeson                    (FromJSON (..), ToJSON (..),
                                                Value (..), object, (.:), (.=))
import qualified Data.Text                     as Text
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
    addEventListener Click onGameCellClicked element

    appendChild gameContainer element

    consoleLog "TEST"

onGameCellClicked :: IO ()
onGameCellClicked = do
    gameState <- getAppState

    consoleLog (Text.show gameState)

    setAppState (over counter (+ 1) gameState)
