module GameCell
    ( GameCell (..)
    , numberOnCellClass
    , hypocentreCellClass
    , closedCellClass
    , openedCellClass
    , openedCellWithMineClass
    , closedCellWithFlagClass
    , closedCellWithWrongFlagClass
    , cellId
    , aroundCells
    ) where

import           Control.Monad  (mzero)
import           Data.Aeson     (FromJSON (..), ToJSON (..), Value (..), object,
                                 (.:), (.=))
import           Data.Text      (Text, pack)
import           Text.Printf    (printf)

import           GameDifficulty (GameDifficulty, screenHeight, screenWidth)

numberOnCellClass :: Int -> Text
numberOnCellClass = pack . printf "gameCell openedCellWithDigit%d"

hypocentreCellClass :: Text
hypocentreCellClass = "gameCell hypocentreCell"

closedCellClass :: Text
closedCellClass = "gameCell closedCell"

openedCellClass :: Text
openedCellClass = "gameCell openedCell"

openedCellWithMineClass :: Text
openedCellWithMineClass = "gameCell openedCellWithMine"

closedCellWithFlagClass :: Text
closedCellWithFlagClass = "gameCell closedCellWithFlag"

closedCellWithWrongFlagClass :: Text
closedCellWithWrongFlagClass = "gameCell closedCellWithWrongFlag"

data GameCell = GameCell Int Int deriving (Show, Eq)

instance ToJSON GameCell where
    toJSON (GameCell x y) = object ["x" .= x, "y" .= y]

instance FromJSON GameCell where
    parseJSON (Object v) =
        GameCell
            <$> v .: "x"
            <*> v .: "y"

    parseJSON _ = mzero

cellId :: GameCell -> Text
cellId (GameCell x y) = pack $ printf "gameCell_%d_%d" x y

aroundCells :: GameDifficulty -> GameCell -> [GameCell]
aroundCells difficulty (GameCell centreX centreY) = possibleCells difficulty
    [ GameCell (centreX - 1) (centreY - 1)
    , GameCell centreX       (centreY - 1)
    , GameCell (centreX + 1) (centreY - 1)
    , GameCell (centreX - 1) centreY
    , GameCell centreX       centreY
    , GameCell (centreX + 1) centreY
    , GameCell (centreX - 1) (centreY + 1)
    , GameCell centreX       (centreY + 1)
    , GameCell (centreX + 1) (centreY + 1)
    ]

possibleCells :: GameDifficulty -> [GameCell] -> [GameCell]
possibleCells difficulty = filter isPossible
    where
        isPossible :: GameCell -> Bool
        isPossible (GameCell x y) =
            let width = screenWidth difficulty
                height = screenHeight difficulty in
                    1 <= x && x <= width && 1 <= y && y <= height
