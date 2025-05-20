{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import           Miso
import           Miso.String
import           Control.Lens

newtype GameState = GameState
    { _counter :: Int
    } deriving (Show, Eq)

makeLenses ''GameState

data GameAction = IncreaseCount
    deriving (Show, Eq)

mainComponent :: Component name GameState GameAction
mainComponent = defaultComponent initState updateState renderHtml

initState :: GameState
initState = GameState 0

updateState :: GameAction -> Effect GameState GameAction
updateState IncreaseCount = do
    state <- get

    put (over counter (+ 5) state)

    batch []

renderHtml :: GameState -> View GameAction
renderHtml state = div_ []
    [ text $ ms (_counter state)
    , button_ [ onClick IncreaseCount ] [ text "INCREASE" ]
    ]

main :: IO ()
main = run  (startComponent mainComponent)

#ifdef WASM
foreign export javascript "hs_start" main :: IO ()
#endif
