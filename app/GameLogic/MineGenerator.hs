module GameLogic.MineGenerator (generateMines) where

import           GameState
import           GameState.Difficulty (Difficulty)

generateMines :: Cell -> Difficulty -> IO [Cell]
generateMines firstCell difficulty = return [firstCell]
