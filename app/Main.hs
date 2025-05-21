{-# LANGUAGE CPP #-}

module Main (main) where

import           Miso

import           GameLogic  (updateGameState)
import           GameScreen (renderGameScreen)
import           GameState  (GameAction, GameState, initialGameState)

mainComponent :: Component name GameState GameAction
mainComponent = defaultComponent initialGameState updateGameState renderHtml

renderHtml :: GameState -> View GameAction
renderHtml state =
    div_ [class_ "gameContainer"]
        [ renderGameScreen state
        ]

main :: IO ()
main = run  (startComponent mainComponent)

#ifdef WASM
foreign export javascript "hs_start" main :: IO ()
#endif
