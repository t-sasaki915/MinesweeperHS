{-# LANGUAGE TemplateHaskell #-}

module GameState
    ( GameState (..)
    , isGameStarted
    , isGameOver
    , gameDifficulty
    , cellsWithMine
    , openedCells
    , flaggedCells
    , isFlagPlacementMode
    , isChordMode
    , initialGameState
    ) where

import           Control.Lens                  (makeLenses, (^.))
import           Control.Monad                 (mzero)
import           Data.Aeson                    (FromJSON (..), ToJSON (..),
                                                Value (..), object, (.:), (.=))
import           Language.JavaScript.Framework (AppState)

import           GameCell                      (GameCell)
import           GameDifficulty                (GameDifficulty)

data GameState = GameState
    { _isGameStarted       :: Bool
    , _isGameOver          :: Bool
    , _gameDifficulty      :: GameDifficulty
    , _cellsWithMine       :: [GameCell]
    , _openedCells         :: [GameCell]
    , _flaggedCells        :: [GameCell]
    , _isFlagPlacementMode :: Bool
    , _isChordMode         :: Bool
    } deriving Show

makeLenses ''GameState

instance ToJSON GameState where
    toJSON gameState =
        object
            [ "isGameStarted"       .= (gameState ^. isGameStarted)
            , "isGameOver"          .= (gameState ^. isGameOver)
            , "gameDifficulty"      .= (gameState ^. gameDifficulty)
            , "cellsWithMine"       .= (gameState ^. cellsWithMine)
            , "openedCells"         .= (gameState ^. openedCells)
            , "flaggedCells"        .= (gameState ^. flaggedCells)
            , "isFlagPlacementMode" .= (gameState ^. isFlagPlacementMode)
            , "isChordMode"         .= (gameState ^. isChordMode)
            ]

instance FromJSON GameState where
    parseJSON (Object v) =
        GameState
            <$> v .: "isGameStarted"
            <*> v .: "isGameOver"
            <*> v .: "gameDifficulty"
            <*> v .: "cellsWithMine"
            <*> v .: "openedCells"
            <*> v .: "flaggedCells"
            <*> v .: "isFlagPlacementMode"
            <*> v .: "isChordMode"

    parseJSON _ = mzero

instance AppState GameState

initialGameState :: GameDifficulty -> GameState
initialGameState difficulty =
    GameState
        { _isGameStarted       = False
        , _isGameOver          = False
        , _gameDifficulty      = difficulty
        , _cellsWithMine       = []
        , _openedCells         = []
        , _flaggedCells        = []
        , _isFlagPlacementMode = False
        , _isChordMode         = False
        }
