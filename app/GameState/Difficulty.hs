module GameState.Difficulty (Difficulty (..), Difficulties (..)) where

class Difficulties a where
    width         :: a -> Int
    height        :: a -> Int
    numberOfMines :: a -> Int

data Difficulty = Easy
                  | Intermediate
                  | Difficult
                  | Impossible
                  deriving (Show, Eq)

instance Difficulties Difficulty where
    width Easy         = 9
    width Intermediate = 16
    width Difficult    = 30
    width Impossible   = 9

    height Easy         = 9
    height Intermediate = 16
    height Difficult    = 16
    height Impossible   = 9

    numberOfMines Easy         = 10
    numberOfMines Intermediate = 40
    numberOfMines Difficult    = 99
    numberOfMines Impossible   = 67
