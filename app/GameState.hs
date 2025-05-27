{-# LANGUAGE TemplateHaskell #-}

module GameState
    ( GameState (..)
    , isGameStarted
    , firstGameCell
    , initialGameState
    ) where

import           Control.Lens                  (makeLenses, (^.))
import           Control.Monad                 (mzero)
import           Data.Aeson                    (FromJSON (..), ToJSON (..),
                                                Value (..), object, (.:), (.=))
import           Language.JavaScript.Framework (AppState)

import           GameCell                      (GameCell)

data GameState = GameState
    { _isGameStarted :: Bool
    , _firstGameCell :: Maybe GameCell
    } deriving Show

makeLenses ''GameState

instance ToJSON GameState where
    toJSON gameState =
        object
            [ "isGameStarted" .= (gameState ^. isGameStarted)
            , "firstGameCell" .= (gameState ^. firstGameCell)
            ]

instance FromJSON GameState where
    parseJSON (Object v) =
        GameState
            <$> v .: "isGameStarted"
            <*> v .: "firstGameCell"

    parseJSON _ = mzero

instance AppState GameState

initialGameState :: GameState
initialGameState =
    GameState
        { _isGameStarted = False
        , _firstGameCell = Nothing
        }
