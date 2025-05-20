{-# LANGUAGE CPP             #-}

module Main (main) where

import           Miso
import           Miso.Lens
import           Miso.String

main :: IO ()
main = run  (startComponent mainComponent)

#ifdef WASM
foreign export javascript "hs_start" main :: IO ()
#endif

newtype GameState = GameState
    { _counter :: Int
    } deriving (Show, Eq)

counter :: Lens GameState Int
counter = lens _counter $ \record field -> record { _counter = field }

data GameAction = IncreaseCount
    deriving (Show, Eq)

mainComponent :: Component name GameState GameAction
mainComponent = defaultComponent initState updateState renderHtml

initState :: GameState
initState = GameState 0

updateState :: GameAction -> Effect GameState GameAction
updateState = \case
    IncreaseCount -> counter += 1

renderHtml :: GameState -> View GameAction
renderHtml state = div_ []
    [ text $ ms (state ^. counter)
    , button_ [ onClick IncreaseCount ] [ text "INCREASE" ]
    ]
