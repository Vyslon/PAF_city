module Main where

import Control.Monad (unless, when)
import Control.Concurrent (threadDelay)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List (foldl')
import Foreign.C.Types (CInt(..))
import SDL
import SDL.Font as TTF  -- Corrected import for SDL.Font
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
import Data.Text (pack)

-- Initialization of all systems
initializeSDL :: IO ()
initializeSDL = do
    SDL.initialize [SDL.InitVideo]
    Font.initialize  -- Assuming Font has an initialize function

cleanupSDL :: IO ()
cleanupSDL = do
    Font.quit  -- Assuming Font has a quit function
    SDL.quit


-- Function to draw all zones
drawZones :: Renderer -> [Sim.Zone] -> IO ()
drawZones renderer zones = mapM_ (drawZone renderer) zones

drawZone :: Renderer -> Sim.Zone -> IO ()
drawZone renderer zone = do
    let color = S.zoneColor zone  -- Assuming zoneColor is a function defined in Sprite.hs
    let area = formeToArea (Sim.zoneForme zone)
    S.createColoredSprite renderer color area  -- Adjust if needed

drawBuilding :: Renderer -> TextureMap -> Sim.Batiment -> IO ()
drawBuilding renderer tmap building = do
    let forme = Sim.getForme building
    let textureId = getTextureIdForBuilding building
    let texture = TM.fetchTexture textureId tmap
    let area = formeToArea forme
    SDL.copy renderer texture Nothing (Just area)  -- Use SDL.copy to render the texture

displayMoney :: Renderer -> Font.Font -> Int -> IO ()
displayMoney renderer font money = do
    let color = V4 255 255 255 255
    textSurface <- Font.renderTextSolid font (pack $ show money ++ " €") color  -- Assuming there's a correct function
    texture <- SDL.createTextureFromSurface renderer textSurface
    SDL.freeSurface textSurface
    texInfo <- SDL.queryTexture texture
    let dstRect = Just $ SDL.Rectangle (SDL.P (V2 10 10)) (V2 (SDL.textureWidth texInfo) (SDL.textureHeight texInfo))
    SDL.copy renderer texture Nothing dstRect
    SDL.destroyTexture texture

-- Main game loop
gameLoop :: (RealFrac a, Show a) => a -> Renderer -> TextureMap -> SpriteMap -> Keyboard -> GameState -> Sim.Ville -> TTF.Font -> Int -> IO ()
gameLoop frameRate renderer tmap smap kbd gameState ville font argent = do
    startTime <- time
    events <- pollEvents
    let kbd' = K.handleEvents events kbd
    let mouseState = MS.handleEventsMousePos events (MS.MyMouse False (-1) (-1))
    clear renderer
    S.displaySprite renderer tmap (SM.fetchSprite (SpriteId "background") smap)
    displayMoney renderer font argent  -- Display money
    present renderer
    endTime <- time
    unless (K.keyPressed KeycodeEscape kbd') $ gameLoop frameRate renderer tmap smap kbd' gameState ville font (argent + 1)  -- Increment money for demonstration

main :: IO ()
main = do
    initializeSDL
    window <- SDL.createWindow (pack "Minijeu") $ SDL.defaultWindow { SDL.windowInitialSize = V2 800 800 }
    renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
    font <- Font.load "assets/Roboto.ttf" 24  -- Check Font for correct function to load fonts
    tmap <- loadBuildingTextures renderer TM.createTextureMap
    smap <- loadSprites renderer tmap SM.createSpriteMap
    let kbd = K.createKeyState
    let ville = Sim.createInitialVille
    let argent = 0
    gameLoop 60 renderer tmap smap kbd ville font argent
    Font.free font  -- Check Font for correct function to free fonts
    cleanupSDL
