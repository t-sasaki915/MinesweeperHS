module GameLogic (updateGameState) where

import           Miso        (Effect, consoleLog, io_)
import           Miso.String (ms)
import           Text.Printf (printf)

import           GameState   (GameAction (..), GameState)

updateGameState :: GameAction -> Effect GameState GameAction
updateGameState (CellClicked x y) =
    io_ $ consoleLog $ ms (printf "Clicked: (%d, %d)" x y :: String)
