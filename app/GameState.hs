{-# OPTIONS_GHC -Wno-partial-fields #-}

module GameState
    ( Cell (..)
    , GameState (..)
    , GameAction (..)
    , initialGameState
    , cellX
    , cellY
    , hasCellMine
    ) where

data Cell = Cell Int Int deriving (Show, Eq)

cellX :: Cell -> Int
cellX (Cell x _) = x

cellY :: Cell -> Int
cellY (Cell _ y) = y

data GameState = NotStarted
               | GameStarted
                    { cellsWithMine :: [Cell]
                    }
    deriving (Show, Eq)

data GameAction = CellClicked Cell
                | UpdateMines [Cell]
    deriving (Show, Eq)

initialGameState :: GameState
initialGameState = NotStarted

hasCellMine :: Cell -> GameState -> Bool
hasCellMine cell state@GameStarted {} = cell `elem` cellsWithMine state
hasCellMine _    NotStarted           = False
