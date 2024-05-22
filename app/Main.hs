import Control.Monad (unless, when)
import Control.Concurrent (threadDelay)
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
import Data.Text (pack)
import qualified SDL.Font as TTF 
import SDL (initialize, InitFlag(InitVideo, InitAudio))

-- Custom initialization function
customInitialize :: IO ()
customInitialize = do
    initialize [InitVideo, InitAudio]  -- Initialize necessary SDL subsystems
    TTF.initialize  -- Initialize the SDL.Font subsystem

cleanup :: IO ()
cleanup = do
    TTF.quit  -- Quit the SDL.Font subsystem
    SDL.quit  -- Quit SDL



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
    S.createColoredSprite renderer color area  -- Adjust if needed

drawBuilding :: Renderer -> TextureMap -> Sim.Batiment -> IO ()
drawBuilding renderer tmap building = do
    let forme = Sim.getForme building
    let textureId = getTextureIdForBuilding building
    let texture = TM.fetchTexture textureId tmap
    let area = formeToArea forme
    SDL.copy renderer texture Nothing (Just area)  -- Use SDL.copy to render the texture


-- Load background image
loadBackgroundSprite :: Renderer -> TextureMap -> SpriteMap -> IO SpriteMap
loadBackgroundSprite renderer tmap smap = do
    let backgroundTextureId = TextureId "background"  -- Assurez-vous que cette ID correspond à une texture chargée dans tmap
    let backgroundArea = S.mkArea 0 0 640 480  -- Taille de l'arrière-plan
    let backgroundSprite = createBuildingSprite backgroundTextureId backgroundArea
    return $ SM.addSprite (SpriteId "background") backgroundSprite smap

-- Load character sprite
loadPerso :: Renderer -> FilePath -> TextureMap -> SpriteMap -> IO (TextureMap, SpriteMap)
loadPerso rdr path tmap smap = do
    tmap' <- TM.loadTexture rdr path (TextureId "perso") tmap
    let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId "perso") (S.mkArea 0 0 200 200)
    let smap' = SM.addSprite (SpriteId "perso") sprite smap
    return (tmap', smap')

-- Function to display the current money in the game
displayMoney :: Renderer -> TTF.Font -> Int -> IO ()
displayMoney renderer font money = do
    let text = pack $ "Money: " ++ show money ++ " €"  -- Prepare text
    let color = V4 255 255 255 255  -- White color
    surface <- TTF.blended font color text  -- Create anti-aliased text surface
    texture <- SDL.createTextureFromSurface renderer surface  -- Create texture from surface
    SDL.freeSurface surface  -- Free the surface after creating the texture
    texInfo <- SDL.queryTexture texture  -- Get texture info to find out dimensions
    let textPos = P (V2 10 10)  -- Define position for the text
    let textRect = Rectangle textPos (V2 (SDL.textureWidth texInfo) (SDL.textureHeight texInfo))
    SDL.copy renderer texture Nothing (Just textRect)  -- Render the texture
    SDL.destroyTexture texture  -- Destroy the texture to clean up


-- Main game loop
gameLoop :: (RealFrac a, Show a) => a -> Renderer -> TextureMap -> SpriteMap -> K.KeyState -> Sim.Ville -> TTF.Font -> Int -> Int -> IO ()
gameLoop frameRate renderer tmap smap kbd ville font argent frameCount = do
    startTime <- time
    events <- pollEvents
    let kbd' = K.handleEvents events kbd
    let mouseState = MS.handleEventsMousePos events (MS.MyMouse False False (-1) (-1))  -- Create initial state of the mouse here
    -- Update the game state, handle input, etc.
    (updatedVille, updatedArgent) <- MS.handleMouseEvents mouseState ville argent renderer tmap

    clear renderer
    -- Draw background
    S.displaySprite renderer tmap (SM.fetchSprite (SpriteId "background") smap)
    -- Draw money
    displayMoney renderer font argent  -- Display money using SDL.Font

    -- Draw zones and buildings from updated Ville
    let zones = Sim.getZones updatedVille
    mapM_ (\zone -> drawZone renderer zone) zones
    let buildings = Sim.getAllBuildings updatedVille
    mapM_ (\building -> drawBuilding renderer tmap building) buildings
    present renderer
    endTime <- time
    let elapsed = endTime - startTime
    let delayTime = max 0 (ceiling (1000 / frameRate - elapsed * 1000))
    threadDelay (delayTime * 1000)  -- delay to cap frame rate
    let newFrameCount = frameCount + 1
    let newArgent = if newFrameCount `mod` 300 == 0
                    then updatedArgent + 100 + (Sim.calculateMoney updatedVille )
                    else updatedArgent
    unless (K.keyPressed KeycodeEscape kbd') $ gameLoop frameRate renderer tmap smap kbd' updatedVille font newArgent newFrameCount

-- Handle mouse click on zones
handleMouseClick :: MyMouse -> [Sim.Zone] -> IO ()
handleMouseClick mouse zones = do
    let mouseX = MS.mouseX mouse
    let mouseY = MS.mouseY mouse
    mapM_ (checkZoneClick (mouseX, mouseY)) zones

-- Check if mouse click is within a zone and print limits
checkZoneClick :: (Int, Int) -> Sim.Zone -> IO ()
checkZoneClick (x, y) zone = do
    let forme = Sim.zoneForme zone
    let (nord, sud, ouest, est) = Sim.limites forme  -- Adjust to use named boundaries correctly
    when (x >= ouest && x <= est && y >= sud && y <= nord) $ do
        putStrLn $ "Zone clicked: " ++ show (Sim.limites forme)

--Pour charger les images, et attribuer à chaque batiment une image
loadBuildingTextures :: Renderer -> TextureMap -> IO TextureMap
loadBuildingTextures renderer initialMap = do
    tmapWithBackground <- TM.loadTexture renderer "assets/background.bmp" (TextureId "background") initialMap
    -- Charger la texture pour l'atelier et mettre à jour la carte de textures
    tmapWithAtelier <- TM.loadTexture renderer "assets/atelier.bmp" (TextureId "atelier") tmapWithBackground

    -- Charger la texture pour la cabane et mettre à jour la carte de textures
    tmapWithCabane <- TM.loadTexture renderer "assets/cabane.bmp" (TextureId "cabane") tmapWithAtelier

    -- Charger la texture pour l'épicerie et mettre à jour la carte de textures
    tmapWithEpicerie <- TM.loadTexture renderer "assets/epicerie.bmp" (TextureId "epicerie") tmapWithCabane

    -- Charger la texture pour le commissariat et mettre à jour la carte de textures finale
    finalMap <- TM.loadTexture renderer "assets/comissariat.bmp" (TextureId "commissariat") tmapWithEpicerie

    return finalMap


-- Function to draw all buildings
drawBuildings :: Renderer -> TextureMap -> [Sim.Batiment] -> IO ()
drawBuildings renderer tmap buildings = mapM_ (drawBuilding renderer tmap) buildings

loadSprite :: Renderer -> FilePath -> SpriteId -> TextureMap -> IO (Sprite, TextureMap)
loadSprite renderer filePath spriteId textureMap = do
    updatedTextureMap <- TM.loadTexture renderer filePath (TextureId $ show spriteId) textureMap
    let sprite = createBuildingSprite (TextureId $ show spriteId) (S.mkArea 0 0 200 200)
    return (sprite, updatedTextureMap)

-- Fonction pour charger les sprites en utilisant les textures déjà chargées
loadSprites :: Renderer -> TextureMap -> SpriteMap -> IO SpriteMap
loadSprites renderer tmap smapInitial = do
    -- Charger les sprites pour différents types de bâtiments
    atelier <- createAndAddSprite renderer tmap (SpriteId "atelier")
    cabane <- createAndAddSprite renderer tmap (SpriteId "cabane")
    epicerie <- createAndAddSprite renderer tmap (SpriteId "epicerie")
    commissariat <- createAndAddSprite renderer tmap (SpriteId "commissariat")

    -- Créer la SpriteMap en ajoutant chaque sprite
    let updatedSmap = foldl' (\smap (id, sprite) -> SM.addSprite id sprite smap) smapInitial [atelier, cabane, epicerie, commissariat]

    -- Créer et ajouter le sprite de background
    let backgroundSprite = createBuildingSprite (TextureId "background") (S.mkArea 0 0 640 480)
    let finalSmap = SM.addSprite (SpriteId "background") backgroundSprite updatedSmap

    return finalSmap


-- Fonction auxiliaire pour créer et ajouter un sprite
createAndAddSprite :: Renderer -> TextureMap -> SpriteId -> IO (SpriteId, Sprite)
createAndAddSprite renderer tmap spriteId = do
    let texture = TM.fetchTexture (TextureId $ show spriteId) tmap
    let sprite = createBuildingSprite (TextureId $ show spriteId) (S.mkArea 0 0 200 200)
    return (spriteId, sprite)


createBuildingSprite :: TextureId -> S.Area -> Sprite
createBuildingSprite textureId area =
    S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage textureId area

-- Function to determine the texture ID based on the building type
getTextureIdForBuilding :: Sim.Batiment -> TextureId
getTextureIdForBuilding (Sim.Cabane _ _ _ _ _) = TextureId "cabane"
getTextureIdForBuilding (Sim.Atelier _ _ _ _ _) = TextureId "atelier"
getTextureIdForBuilding (Sim.Epicerie _ _ _ _ _) = TextureId "epicerie"
getTextureIdForBuilding (Sim.Commissariat _ _ _) = TextureId "commissariat"



main :: IO ()
main = do
    customInitialize  -- Use the custom initialization for SDL and fonts
    window <- createWindow (pack "Minijeu") $ defaultWindow { windowInitialSize = V2 800 800 }
    renderer <- createRenderer window (-1) defaultRenderer
    font <- TTF.load "assets/Roboto.ttf" 24  -- Load the font; specify the correct path and size
    
    tmap <- loadBuildingTextures renderer TM.createTextureMap
    smap <- loadSprites renderer tmap SM.createSpriteMap
    let kbd = K.createKeyState
    let ville = Sim.createInitialVille  -- Initialize your city here
    let argent = 0

    -- Check if the city respects the property
    let villeRespectsProperty = Sim.prop_ville ville
    putStrLn $ "Does the city respect the property? " ++ show villeRespectsProperty

    -- Proceed with the game loop
    gameLoop 60 renderer tmap smap kbd ville font argent 0  -- Pass the font to the game loop
    
