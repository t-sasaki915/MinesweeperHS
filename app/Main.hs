{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import           Miso
import           Control.Lens

import           Textures     (Texture (..), Textures (..))

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
renderHtml _ =
    div_ [class_ "gameContainer"]
        [ div_ [class_ "gameScreen"] $
            flip map [1..9] $ const $
                div_ [class_ "gameRow"] $
                    flip map [1..9] $ const $
                        div_ [class_ "gameCell"]
                            [ textureSvg ClosedCellWithFlag
                            ]
        ]

main :: IO ()
main = run  (startComponent mainComponent)

#ifdef WASM
foreign export javascript "hs_start" main :: IO ()
#endif
