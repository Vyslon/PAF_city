
module Model where

import SDL

import Keyboard (Keyboard)
import qualified Keyboard as K

import Mouse (MyMouse)
import qualified Mouse as M

data GameState = GameState { persoX :: Int
                           , persoY :: Int
                           , speed :: Int }
  deriving (Show)


initGameState :: GameState
initGameState = GameState 200 300 4

moveLeft :: GameState -> GameState
moveLeft (GameState px py sp) = (GameState (px - sp) py sp)

moveRight :: GameState -> GameState
moveRight (GameState px py sp) = (GameState (px + sp) py sp)
                              
moveUp :: GameState -> GameState
moveUp (GameState px py sp) = (GameState px (py - sp) sp)

moveDown :: GameState -> GameState
moveDown (GameState px py sp) = (GameState px (py + sp) sp)

click :: MyMouse -> GameState -> Integer -> Integer -> GameState
click mse gstate width height = TODO



gameStep :: RealFrac a => GameState -> Keyboard -> MyMouse -> Integer -> Integer -> a -> GameState
gameStep gstate kbd mse width height deltaTime =
  let modif = (if K.keypressed KeycodeLeft kbd
               then moveLeft else id)
              .
              (if K.keypressed KeycodeRight kbd
               then moveRight else id)
              .
              (if K.keypressed KeycodeUp kbd
               then moveUp else id)
              .
              (if K.keypressed KeycodeDown kbd
               then moveDown else id)

              
              --(if M.mouseActif mse
              -- then click mse gstate else id)

  in modif gstate
