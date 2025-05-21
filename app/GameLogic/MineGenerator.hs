module GameLogic.MineGenerator (generateMines) where

import           GameState

generateMines :: Cell -> GameState -> IO [Cell]
generateMines firstCell state = return [firstCell]
