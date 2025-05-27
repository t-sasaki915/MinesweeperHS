module GameDifficulty
    ( GameDifficulty (..)
    , defaultGameDifficulty
    , gameDifficultyFromText
    , screenWidth
    , screenHeight
    , numberOfMines
    ) where

import           Control.Monad (mzero)
import           Data.Aeson    (FromJSON (..), ToJSON (..), Value (..))
import           Data.Text     (Text)
import qualified Data.Text     as Text

data GameDifficulty = Easy
                    | Intermediate
                    | Hard
                    | Impossible
                    deriving (Show, Eq)

instance ToJSON GameDifficulty where
    toJSON Easy         = String "Easy"
    toJSON Intermediate = String "Intermediate"
    toJSON Hard         = String "Hard"
    toJSON Impossible   = String "Impossible"

instance FromJSON GameDifficulty where
    parseJSON (String "Easy")         = return Easy
    parseJSON (String "Intermediate") = return Intermediate
    parseJSON (String "Hard")         = return Hard
    parseJSON (String "Impossible")   = return Impossible

    parseJSON _                       = mzero

defaultGameDifficulty :: GameDifficulty
defaultGameDifficulty = Easy

gameDifficultyFromText :: Text -> Maybe GameDifficulty
gameDifficultyFromText txt =
    case Text.toLower txt of
        "easy"         -> Just Easy
        "intermediate" -> Just Intermediate
        "hard"         -> Just Hard
        "impossible"   -> Just Impossible
        _              -> Nothing

screenWidth :: GameDifficulty -> Int
screenWidth Easy         = 9
screenWidth Intermediate = 16
screenWidth Hard         = 30
screenWidth Impossible   = 9

screenHeight :: GameDifficulty -> Int
screenHeight Easy         = 9
screenHeight Intermediate = 16
screenHeight Hard         = 16
screenHeight Impossible   = 9

numberOfMines :: GameDifficulty -> Int
numberOfMines Easy         = 10
numberOfMines Intermediate = 40
numberOfMines Hard         = 99
numberOfMines Impossible   = 67
