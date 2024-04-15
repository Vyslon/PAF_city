module Main where

import Control.Monad (unless)
import Control.Concurrent (threadDelay)
import Data.Text (pack)  -- Import the pack function from Data.Text
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List (foldl')
import Foreign.C.Types (CInt(..))
import SDL
import SDL.Time (time, delay)
import Linear (V4(..))
import TextureMap (TextureMap, TextureId(..))
import qualified TextureMap as TM
import Sprite (Sprite)
import qualified Sprite as S
import SpriteMap (SpriteMap, SpriteId(..))
import qualified SpriteMap as SM
import Keyboard (Keyboard)
import qualified Keyboard as K
import Mouse (MyMouse)
import qualified Mouse as MS
import Model (GameState)
import qualified Model as M
import qualified Data.Map as Map
import qualified SimCity as Sim

-- Function to convert Forme to SDL Area
formeToArea :: Sim.Forme -> S.Area
formeToArea (Sim.Rectangle (Sim.C x y) w h) = S.mkArea (fromIntegral x) (fromIntegral y) (fromIntegral w) (fromIntegral h)
formeToArea _ = error "Unsupported Forme type for conversion to Area"

-- Function to draw all zones
drawZones :: Renderer -> [Sim.Zone] -> IO ()
drawZones renderer zones = mapM_ (drawZone renderer) zones

drawZone :: Renderer -> Sim.Zone -> IO ()
drawZone renderer zone = do
    let color = S.zoneColor zone  -- Assuming zoneColor is a function defined in Sprite.hs
    let area = formeToArea (Sim.zoneForme zone)
    S.createColoredSprite renderer color area

-- Load background image
loadBackground :: Renderer -> FilePath -> TextureMap -> SpriteMap -> IO (TextureMap, SpriteMap)
loadBackground rdr path tmap smap = do
    tmap' <- TM.loadTexture rdr path (TextureId "background") tmap
    let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId "background") (S.mkArea 0 0 640 480)
    let smap' = SM.addSprite (SpriteId "background") sprite smap
    return (tmap', smap')

-- Load character sprite
loadPerso :: Renderer -> FilePath -> TextureMap -> SpriteMap -> IO (TextureMap, SpriteMap)
loadPerso rdr path tmap smap = do
    tmap' <- TM.loadTexture rdr path (TextureId "perso") tmap
    let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId "perso") (S.mkArea 0 0 200 200)
    let smap' = SM.addSprite (SpriteId "perso") sprite smap
    return (tmap', smap')

-- Main game loop
gameLoop :: (RealFrac a, Show a) => a -> Renderer -> TextureMap -> SpriteMap -> Keyboard -> GameState -> [Sim.Zone] -> IO ()
gameLoop frameRate renderer tmap smap kbd gameState zones = do
    startTime <- time
    events <- pollEvents
    let kbd' = K.handleEvents events kbd
    let mse = MS.handleEventsMousePos events (MS.MyMouse False (-1) (-1))
    clear renderer
    -- Draw background
    S.displaySprite renderer tmap (SM.fetchSprite (SpriteId "background") smap)
    -- Draw character
    --S.displaySprite renderer tmap (S.moveTo (SM.fetchSprite (SpriteId "perso") smap)
      --                              (fromIntegral (M.persoX gameState))
        --                            (fromIntegral (M.persoY gameState)))
    -- Draw zones
    drawZones renderer zones
    present renderer
    endTime <- time
    let refreshTime = endTime - startTime
    let delayTime = floor (((1.0 / frameRate) - refreshTime) * 1000)
    threadDelay $ delayTime * 1000 -- microseconds
    let deltaTime = endTime - startTime
    unless (K.keypressed KeycodeEscape kbd') (gameLoop frameRate renderer tmap smap kbd' gameState zones)

-- Main function to setup the game
main :: IO ()
main = do
  initializeAll
  window <- createWindow (pack "Minijeu") $ defaultWindow { windowInitialSize = V2 640 480 }
  renderer <- createRenderer window (-1) defaultRenderer
  -- Load background and character sprites
  (tmap, smap) <- loadBackground renderer "assets/background.bmp" TM.createTextureMap SM.createSpriteMap
  (tmap', smap') <- loadPerso renderer "assets/perso.bmp" tmap smap
  -- Initialize game state and keyboard
  let gameState = M.initGameState
  let kbd = K.createKeyboard
  -- Create a sample list of zones for testing
  let zones = [Sim.ZR (Sim.Rectangle (Sim.C 100 150) 300 200) [],Sim.ZI (Sim.Rectangle (Sim.C 500 150) 300 200) [],Sim.ZC (Sim.Rectangle (Sim.C 100 400) 300 200) [],Sim.Route (Sim.Rectangle (Sim.C 0 100) 300 50),Sim.Eau (Sim.Rectangle (Sim.C 0 550) 900 50),Sim.Admin (Sim.Rectangle (Sim.C 400 400) 100 100) (Sim.Commissariat (Sim.Rectangle (Sim.C 400 400) 100 100) (Sim.C 400 400))]

  -- Launch the game loop with zones
  gameLoop 60 renderer tmap' smap' kbd gameState zones
