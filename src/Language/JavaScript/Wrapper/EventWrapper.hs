module Language.JavaScript.Wrapper.EventWrapper
    ( EventType (..)
    , addEventListener
    , addEventListenerNoState
    ) where

import           Control.Monad.Trans.State.Strict             (StateT,
                                                               execStateT)
import           GHC.JS.Foreign.Callback                      (asyncCallback)
import           Language.JavaScript.Framework                (AppState)
import           Language.JavaScript.Framework.AppState       (getAppState,
                                                               setAppState)
import           Language.JavaScript.Wrapper.ElementWrapper   (Element (..))
import           Language.JavaScript.Wrapper.Internal.Foreign

data EventType = Click
               | RightClick
               deriving Show

addEventListenerNoState :: EventType -> IO () -> Element -> IO ()
addEventListenerNoState eventType listener (Element element) =
    asyncCallback listener >>= \callback ->
        case eventType of
            Click      -> addLeftClickEventListener_ callback element
            RightClick -> addRightClickEventListener_ callback element

addEventListener :: AppState a => EventType -> StateT a IO () -> Element -> IO ()
addEventListener eventType stateListener element =
    let listener = getAppState >>= execStateT stateListener >>= setAppState in
        addEventListenerNoState eventType listener element
