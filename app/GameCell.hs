module GameCell
    ( GameCellStatus (..)
    , GameCell (..)
    , numberOnCellClass
    , hypocentreCellClass
    , closedCellClass
    , openedCellClass
    , openedCellWithMineClass
    , cellId
    , aroundCells
    ) where

import           Control.Monad  (mzero)
import           Data.Aeson     (FromJSON (..), ToJSON (..), Value (..), object,
                                 (.:), (.=))
import           Data.Text      (Text, pack)
import           GameDifficulty (GameDifficulty, screenHeight, screenWidth)
import           Text.Printf    (printf)

data GameCellStatus = IsMine
                    | One
                    | Two
                    | Three
                    | Four
                    | Five
                    | Six
                    | Seven
                    | Eight
                    | Zero
                    deriving Eq

numberOnCellClass :: GameCellStatus -> Maybe Text
numberOnCellClass One   = Just "gameCell openedCellWithDigit1"
numberOnCellClass Two   = Just "gameCell openedCellWithDigit2"
numberOnCellClass Three = Just "gameCell openedCellWithDigit3"
numberOnCellClass Four  = Just "gameCell openedCellWithDigit4"
numberOnCellClass Five  = Just "gameCell openedCellWithDigit5"
numberOnCellClass Six   = Just "gameCell openedCellWithDigit6"
numberOnCellClass Seven = Just "gameCell openedCellWithDigit7"
numberOnCellClass Eight = Just "gameCell openedCellWithDigit8"
numberOnCellClass _     = Nothing

hypocentreCellClass :: Text
hypocentreCellClass = "gameCell hypocentreCell"

closedCellClass :: Text
closedCellClass = "gameCell closedCell"

openedCellClass :: Text
openedCellClass = "gameCell openedCell"

openedCellWithMineClass :: Text
openedCellWithMineClass = "gameCell openedCellWithMine"

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
