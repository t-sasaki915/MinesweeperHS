module Language.JavaScript.Wrapper.EventWrapper
    ( EventType (..)
    , eventTypeName
    , addEventListener
    , addEventListenerWithState
    ) where

import           Control.Monad.Trans.State.Strict             (StateT,
                                                               execStateT)
import           GHC.JS.Foreign.Callback                      (asyncCallback)
import           GHC.JS.Prim                                  (JSVal,
                                                               toJSString)
import           Language.JavaScript.Framework                (AppState,
                                                               getAppState,
                                                               setAppState)
import           Language.JavaScript.Wrapper.ElementWrapper   (Element (..))
import           Language.JavaScript.Wrapper.Internal.Foreign (addEventListener_)

data EventType = Click
               deriving Show

eventTypeName :: EventType -> JSVal
eventTypeName Click = toJSString "click"

addEventListener :: EventType -> IO () -> Element -> IO ()
addEventListener eventType listener (Element element) =
    asyncCallback listener >>= \callback ->
        addEventListener_ (eventTypeName eventType) callback element

addEventListenerWithState :: AppState a => EventType -> StateT a IO () -> Element -> IO ()
addEventListenerWithState eventType stateListener element =
    let listener = getAppState >>= execStateT stateListener >>= setAppState in
        addEventListener eventType listener element
