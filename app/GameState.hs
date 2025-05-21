{-# LANGUAGE TemplateHaskell #-}

module GameState
    ( GameState (..)
    , GameAction (..)
    , initialGameState
    ) where

import           Control.Lens (makeLenses)

data GameState = GameState
    deriving (Show, Eq)

makeLenses ''GameState

data GameAction = CellClicked Int Int
    deriving (Show, Eq)

initialGameState :: GameState
initialGameState = GameState
