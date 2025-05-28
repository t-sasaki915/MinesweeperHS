{-# LANGUAGE TemplateHaskell #-}

module GameState
    ( GameState (..)
    , isGameStarted
    , gameDifficulty
    , cellsWithMine
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
    { _isGameStarted  :: Bool
    , _gameDifficulty :: GameDifficulty
    , _cellsWithMine  :: [GameCell]
    } deriving Show

makeLenses ''GameState

instance ToJSON GameState where
    toJSON gameState =
        object
            [ "isGameStarted"  .= (gameState ^. isGameStarted)
            , "gameDifficulty" .= (gameState ^. gameDifficulty)
            , "cellsWithMine"  .= (gameState ^. cellsWithMine)
            ]

instance FromJSON GameState where
    parseJSON (Object v) =
        GameState
            <$> v .: "isGameStarted"
            <*> v .: "gameDifficulty"
            <*> v .: "cellsWithMine"

    parseJSON _ = mzero

instance AppState GameState

initialGameState :: GameDifficulty -> GameState
initialGameState difficulty =
    GameState
        { _isGameStarted  = False
        , _gameDifficulty = difficulty
        , _cellsWithMine  = []
        }
