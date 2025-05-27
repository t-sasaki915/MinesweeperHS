{-# LANGUAGE TemplateHaskell #-}

module GameState
    ( GameState (..)
    , isGameStarted
    , firstGameCell
    , gameDifficulty
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
    , _firstGameCell  :: Maybe GameCell
    , _gameDifficulty :: GameDifficulty
    } deriving Show

makeLenses ''GameState

instance ToJSON GameState where
    toJSON gameState =
        object
            [ "isGameStarted"  .= (gameState ^. isGameStarted)
            , "firstGameCell"  .= (gameState ^. firstGameCell)
            , "gameDifficulty" .= (gameState ^. gameDifficulty)
            ]

instance FromJSON GameState where
    parseJSON (Object v) =
        GameState
            <$> v .: "isGameStarted"
            <*> v .: "firstGameCell"
            <*> v .: "gameDifficulty"

    parseJSON _ = mzero

instance AppState GameState

initialGameState :: GameDifficulty -> GameState
initialGameState difficulty =
    GameState
        { _isGameStarted  = False
        , _firstGameCell  = Nothing
        , _gameDifficulty = difficulty
        }
