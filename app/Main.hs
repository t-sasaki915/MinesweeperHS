{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

module Main (main) where

import           Yesod

newtype GameState = GameState
    { testValue :: Int
    }

mkYesod "GameState" [parseRoutes|
/ HomeR GET
|]

instance Yesod GameState

getHomeR :: Handler Html
getHomeR = do
    state <- getYesod
    defaultLayout $ do
        setTitle "Minesweeper"
        [whamlet|
            <p>#{testValue state}</p>
        |]

main :: IO ()
main = warp 8080 (GameState 0)
