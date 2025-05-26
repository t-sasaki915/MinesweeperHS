{-# LANGUAGE CPP #-}

module Main (main) where

import           Miso

import           GameLogic            (updateGameState)
import           GameScreen           (renderGameScreen)
import           GameState            (GameAction, GameState, initialGameState)
import           GameState.Difficulty (Difficulty (Easy))

mainComponent :: Component name GameState GameAction
mainComponent = defaultComponent initialGameState (updateGameState Easy) (renderHtml Easy)

renderHtml :: Difficulty -> GameState -> View GameAction
renderHtml difficulty state =
    div_ [class_ "gameContainer"]
        [ renderGameScreen difficulty state
        ]

main :: IO ()
main = run  (startComponent mainComponent)

#ifdef WASM
foreign export javascript "hs_start" main :: IO ()
#endif
