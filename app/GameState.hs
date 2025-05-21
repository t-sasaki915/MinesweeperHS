{-# LANGUAGE TemplateHaskell #-}

module GameState
    ( Cell (..)
    , GameState (..)
    , GameAction (..)
    , initialGameState
    , cellX
    , cellY
    , difficulty
    , isGameStarted
    , cellsWithMine
    , hasCellMine
    ) where

import           Control.Lens         (makeLenses, (^.))

import           GameState.Difficulty (Difficulty (Easy))

data Cell = Cell Int Int deriving (Show, Eq)

cellX :: Cell -> Int
cellX (Cell x _) = x

cellY :: Cell -> Int
cellY (Cell _ y) = y

data GameState = GameState
    { _difficulty    :: Difficulty
    , _isGameStarted :: Bool
    , _cellsWithMine :: [Cell]
    }
    deriving (Show, Eq)

makeLenses ''GameState

data GameAction = CellClicked Cell
                | UpdateMines [Cell]
    deriving (Show, Eq)

initialGameState :: GameState
initialGameState = GameState
    { _difficulty    = Easy
    , _isGameStarted = False
    , _cellsWithMine = []
    }

hasCellMine :: Cell -> GameState -> Bool
hasCellMine cell state = cell `elem` state ^. cellsWithMine
