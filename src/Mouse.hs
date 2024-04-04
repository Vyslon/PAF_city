
module Mouse where

import SDL

import Data.List (foldl')

import Data.Set (Set)
import qualified Data.Set as S

data MyMouse = MyMouse { actif :: Bool, mouseX :: Int
                   , mouseY :: Int }

-- ((mouseButtonEventButton mbep) == ButtonLeft) && 

handleEventMousePos :: Event -> MyMouse -> MyMouse 
handleEventMousePos event _ =
  case eventPayload event of
    MouseButtonEvent mbep ->
        if (mouseButtonEventButton mbep) == ButtonLeft then
            if mouseButtonEventMotion mbep == Pressed
                then  let (P (V2 x y)) = mouseButtonEventPos mbep in MyMouse True (fromIntegral x) (fromIntegral y)
            else -- click released
                let (P (V2 x y)) = mouseButtonEventPos mbep in MyMouse False (fromIntegral x) (fromIntegral y)
        else MyMouse False (-1) (-1)
    _ -> MyMouse False (-1) (-1)

handleEventsMousePos :: [Event] -> MyMouse -> MyMouse
handleEventsMousePos events mse = foldl' (flip handleEventMousePos) mse events

mouseActif :: MyMouse -> Bool
mouseActif (MyMouse res _ _) = res