{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import           Control.Lens                     (makeLenses, over, (^.))
import           Control.Monad                    (forM_, mzero)
import           Control.Monad.Trans.Class        (lift)
import           Control.Monad.Trans.State.Strict (StateT, execStateT, get, put)
import           Data.Aeson                       (FromJSON (..), ToJSON (..),
                                                   Value (..), object, (.:),
                                                   (.=))
import qualified Data.Text                        as Text
import           Language.JavaScript.Framework
import           Language.JavaScript.Wrapper
import           Text.Printf                      (printf)

data Cell = Cell Int Int deriving (Show, Eq)

data GameState = GameState
    { _counter :: Int
    } deriving Show

makeLenses ''GameState

instance ToJSON GameState where
    toJSON gameState =
        object
            [ "counter" .= (gameState ^. counter)
            ]

instance FromJSON GameState where
    parseJSON (Object v) =
        GameState
            <$> v .: "counter"

    parseJSON _ = mzero

instance AppState GameState

main :: IO ()
main = do
    initialiseAppState (GameState { _counter = 0 })

    gameContainer <- getElementById "gameContainer"

    forM_ [1..9] $ \y -> do
        rowElem <- createElement Div
        setElementClassName "gameRow" rowElem

        forM_ [1..9] $ \x -> do
            let cell = Cell x y

            cellElem <- createElement Div

            setElementId (Text.pack $ printf "gameCell_%d_%d" x y) cellElem
            setElementClassName "gameCell closedCell" cellElem
            addEventListenerWithState Click (onGameCellClicked cell) cellElem

            appendChild rowElem cellElem

        appendChild gameContainer rowElem

onGameCellClicked :: Cell -> StateT GameState IO ()
onGameCellClicked clickedCell = do
    lift $ consoleLog (Text.show clickedCell)

    get >>= put . over counter (+ 1)
