module GameCell (GameCell (..)) where

import           Control.Monad (mzero)
import           Data.Aeson    (FromJSON (..), ToJSON (..), Value (..), object,
                                (.:), (.=))

data GameCell = GameCell Int Int deriving (Show, Eq)

instance ToJSON GameCell where
    toJSON (GameCell x y) = object ["x" .= x, "y" .= y]

instance FromJSON GameCell where
    parseJSON (Object v) =
        GameCell
            <$> v .: "x"
            <*> v .: "y"

    parseJSON _ = mzero
