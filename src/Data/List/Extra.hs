module Data.List.Extra (cons) where

cons :: a -> [a] -> [a]
cons x xs = xs ++ [x]
