module Keyboard where

import SDL
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List (foldl')

type Keyboard = Set Keycode

data KeyState = KeyState {
    keyboard :: Keyboard,
    keyRWasReleased :: Bool
}

createKeyState :: KeyState
createKeyState = KeyState {
    keyboard = Set.empty,
    keyRWasReleased = True
}

handleKeyEvent :: Event -> KeyState -> KeyState
handleKeyEvent event keyState =
  case eventPayload event of
    KeyboardEvent keyboardEvent ->
      let keycode = keysymKeycode (keyboardEventKeysym keyboardEvent)
          motion = keyboardEventKeyMotion keyboardEvent
          keyStateUpdated = updateKeyState keycode motion keyState
      in keyStateUpdated
    _ -> keyState

updateKeyState :: Keycode -> InputMotion -> KeyState -> KeyState
updateKeyState keycode motion keyState =
    case motion of
        Pressed -> keyState { keyboard = Set.insert keycode (keyboard keyState) }
        Released -> keyState {
            keyboard = Set.delete keycode (keyboard keyState),
            keyRWasReleased = if keycode == KeycodeR then True else keyRWasReleased keyState
        }

handleEvents :: [Event] -> KeyState -> KeyState
handleEvents events keyState = foldl' (flip handleKeyEvent) keyState events

triggerRAction :: KeyState -> Bool
triggerRAction keyState =
    Set.member KeycodeR (keyboard keyState) && keyRWasReleased keyState

-- Updated to use KeyState
keyPressed :: Keycode -> KeyState -> Bool
keyPressed kc keyState = Set.member kc (keyboard keyState)
