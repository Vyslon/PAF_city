import Control.Monad (unless, when)
import Control.Concurrent (threadDelay)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List (foldl')
import Foreign.C.Types (CInt(..))
import SDL 
import SDL.Time (time, delay)
import Data.Maybe (fromMaybe)
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
import Data.IORef
import System.IO.Unsafe (unsafePerformIO)

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
drawZones :: Renderer -> TextureMap -> [Sim.Zone] -> IO ()
drawZones renderer tmap zones = mapM_ (drawZone renderer tmap) zones




-- drawZone :: Renderer -> Sim.Zone -> IO ()
-- drawZone renderer zone = do
--     let color = S.zoneColor zone  -- Assuming zoneColor is a function defined in Sprite.hs
--     let area = formeToArea (Sim.zoneForme zone)
--     S.createColoredSprite renderer color area  -- Adjust if needed

drawZone :: Renderer -> TextureMap -> Sim.Zone -> IO ()
drawZone renderer tmap zone = do
    let forme = Sim.zoneForme zone
    let textureId = getTextureIdForZone zone
    let texture = TM.fetchTexture textureId tmap
    let area = formeToArea forme
    SDL.copy renderer texture Nothing (Just area)  -- Use SDL.copy to render the texture

drawBuilding :: Renderer -> TextureMap -> Sim.Batiment -> IO ()
drawBuilding renderer tmap building = do
    let forme = Sim.getForme building
    let textureId = getTextureIdForBuilding building
    let texture = TM.fetchTexture textureId tmap
    let area = formeToArea forme
    SDL.copy renderer texture Nothing (Just area)  -- Use SDL.copy to render the texture

drawCitizen :: Renderer -> TextureMap -> Sim.Citoyen -> IO ()
drawCitizen renderer tmap citizen = do
    let textureId = getTextureIdForCitoyen citizen
    let texture = TM.fetchTexture textureId tmap
    let area = getCitizenArea citizen  -- Calculate the area for this citizen
    SDL.copy renderer texture Nothing (Just area)

-- Helper function to calculate the SDL area from citizen's position
getCitizenArea :: Sim.Citoyen -> S.Area
getCitizenArea citizen = 
    let (Sim.C x y) = Sim.citizenCoord citizen  -- Assume we can get the coordinates directly
    in S.mkArea (fromIntegral x) (fromIntegral y) 30 80  -- Width = 30, Height = 80

-- Load background image
loadBackgroundSprite :: Renderer -> TextureMap -> SpriteMap -> IO SpriteMap
loadBackgroundSprite renderer tmap smap = do
    let backgroundTextureId = TextureId "background"  -- Assurez-vous que cette ID correspond à une texture chargée dans tmap
    let backgroundArea = S.mkArea 0 0 800 800  -- Taille de l'arrière-plan
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


-- Function to display the number of immigrants, emigrants, and habitants
displayCitizenCount :: Renderer -> TTF.Font -> Sim.Ville -> IO ()
displayCitizenCount renderer font ville = do
    let (immigrants, emigrants, habitants) = Sim.countCitizens(ville)
    let text = pack $ "Immigrants: " ++ show immigrants ++ "\nEmigrants: " ++ show emigrants ++ "\nHabitants: " ++ show habitants
    let color = V4 0 0 0 255  -- Black color for text
    surface <- TTF.blended font color text  -- Create anti-aliased text surface
    texture <- SDL.createTextureFromSurface renderer surface  -- Create texture from surface
    SDL.freeSurface surface  -- Free the surface after creating the texture
    texInfo <- SDL.queryTexture texture  -- Get texture info to find out dimensions

    let textWidth = SDL.textureWidth texInfo
    let textHeight = SDL.textureHeight texInfo
    let padding = 10
    let panelWidth = textWidth + 2 * padding
    let panelHeight = textHeight + 2 * padding

    let panelPos = P (V2 (800 - panelWidth - 10) 10)  -- Define position for the panel in the top-right corner
    let panelRect = Rectangle panelPos (V2 panelWidth panelHeight)

    let textPos = P (V2 (800 - textWidth - padding - 10) (padding + 10))  -- Define position for the text inside the panel
    let textRect = Rectangle textPos (V2 textWidth textHeight)

    -- Draw white background panel
    rendererDrawColor renderer $= V4 255 255 255 255  -- White color for panel
    fillRect renderer (Just panelRect)

    -- Render the text
    SDL.copy renderer texture Nothing (Just textRect)
    SDL.destroyTexture texture  -- Destroy the texture to clean up

displayMouseCoordinates :: Renderer -> TTF.Font -> MyMouse -> IO ()
displayMouseCoordinates renderer font mouse = do
    let text = pack $ "Coordonnées souris: [X = " ++ show (MS.mouseX mouse) ++ ", Y = " ++ show (MS.mouseY mouse) ++ "]"
    let color = V4 0 0 0 255  -- Black color for text
    surface <- TTF.blended font color text  -- Create anti-aliased text surface
    texture <- SDL.createTextureFromSurface renderer surface  -- Create texture from surface
    SDL.freeSurface surface  -- Free the surface after creating the texture
    texInfo <- SDL.queryTexture texture  -- Get texture info to find out dimensions

    let textWidth = SDL.textureWidth texInfo
    let textHeight = SDL.textureHeight texInfo
    let padding = 10
    let panelWidth = textWidth + 2 * padding
    let panelHeight = textHeight + 2 * padding

    let panelPos = P (V2 (800 - panelWidth - 10) 50)  -- Define position for the panel in the top-right corner
    let panelRect = Rectangle panelPos (V2 panelWidth panelHeight)

    let textPos = P (V2 (800 - textWidth - padding - 10) (padding + 50))  -- Define position for the text inside the panel
    let textRect = Rectangle textPos (V2 textWidth textHeight)

    -- Draw white background panel
    rendererDrawColor renderer $= V4 255 255 255 255  -- White color for panel
    fillRect renderer (Just panelRect)

    -- Render the text
    SDL.copy renderer texture Nothing (Just textRect)
    SDL.destroyTexture texture  -- Destroy the texture to clean up

-- Main game loop
gameLoop :: (RealFrac a, Show a) => a -> Renderer -> TextureMap -> SpriteMap -> K.KeyState -> Sim.Ville -> TTF.Font -> Int -> Int -> Sim.CitId -> Sim.BatId -> Maybe MS.MyMouse -> IO ()
gameLoop frameRate renderer tmap smap kbd ville font argent frameCount citId batId currentMouseState = do
    startTime <- time
    events <- pollEvents
    let kbd' = K.handleEvents events kbd
    let mouseState = MS.handleEventsMousePos events (fromMaybe (MS.MyMouse False False (-1) (-1)) currentMouseState)

    -- Update the game state based on mouse events
    (updatedVille, newBatId, updatedArgent) <- MS.handleMouseEvents mouseState ville argent renderer tmap citId batId

    let villeWithUpdatedCitizens = Sim.updateCitizens updatedVille

    clear renderer
    -- Draw background
    S.displaySprite renderer tmap (SM.fetchSprite (SpriteId "background") smap)
    -- Draw all zones
    let zones = Sim.getZones villeWithUpdatedCitizens
    mapM_ (\zone -> drawZone renderer tmap zone) zones
    -- Draw all buildings
    let buildings = Sim.getAllBuildings villeWithUpdatedCitizens
    mapM_ (\building -> drawBuilding renderer tmap building) buildings
    -- Draw all citizens
    let citizens = Sim.getAllCitizens villeWithUpdatedCitizens
    mapM_ (\citizen -> drawCitizen renderer tmap citizen) citizens

    -- Display current money
    displayMoney renderer font updatedArgent
    -- Display citizen counts
    displayCitizenCount renderer font villeWithUpdatedCitizens
    -- Display mouse coordinates
    displayMouseCoordinates renderer font mouseState

    present renderer
    endTime <- time
    let elapsed = endTime - startTime
    let delayTime = max 0 (ceiling (1000 / frameRate - elapsed * 1000))
    threadDelay (delayTime * 1000)

    let newFrameCount = frameCount + 1
    let newArgent = if newFrameCount `mod` 500 == 0
                    then updatedArgent + 100 + (Sim.calculateMoney villeWithUpdatedCitizens)
                    else updatedArgent

    -- Add immigrants every 500 frames
    let (newVille, newCitId) = if newFrameCount `mod` 500 == 0
                               then Sim.addImmigrants 1 villeWithUpdatedCitizens citId
                               else (villeWithUpdatedCitizens, citId)
    let finalVille = Sim.updateCitizens newVille

    unless (K.keyPressed KeycodeEscape kbd') $ gameLoop frameRate renderer tmap smap kbd' finalVille font newArgent newFrameCount newCitId newBatId (Just mouseState)



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
    tmapWithAtelier <- TM.loadTexture renderer "assets/atelier.bmp" (TextureId "atelier") tmapWithBackground
    tmapWithCabane <- TM.loadTexture renderer "assets/cabane.bmp" (TextureId "cabane") tmapWithAtelier
    tmapWithEpicerie <- TM.loadTexture renderer "assets/epicerie.bmp" (TextureId "epicerie") tmapWithCabane
    tmapWithComico <- TM.loadTexture renderer "assets/comissariat.bmp" (TextureId "commissariat") tmapWithEpicerie
    tmapWithImmigrant <- TM.loadTexture renderer "assets/Immigrant.bmp" (TextureId "immigrant") tmapWithComico
    tmapWithHabitant <- TM.loadTexture renderer "assets/Habitant.bmp" (TextureId "habitant") tmapWithImmigrant
    tmapWithEmigrant <- TM.loadTexture renderer "assets/Emigrant.bmp" (TextureId "emigrant") tmapWithHabitant
    tmapWithRoute <- TM.loadTexture renderer "assets/route.bmp" (TextureId "route") tmapWithEmigrant
    tmapWithCable <- TM.loadTexture renderer "assets/cable.bmp" (TextureId "cable") tmapWithRoute
    tmapWithZR <- TM.loadTexture renderer "assets/ZR.bmp" (TextureId "ZR") tmapWithCable
    tmapWithZI <- TM.loadTexture renderer "assets/ZI.bmp" (TextureId "ZI") tmapWithZR
    tmapWithZC <- TM.loadTexture renderer "assets/ZC.bmp" (TextureId "ZC") tmapWithZI
    tmapWithZE <- TM.loadTexture renderer "assets/ZE.bmp" (TextureId "ZE") tmapWithZC
    tmapWithAdmin <- TM.loadTexture renderer "assets/admin.bmp" (TextureId "admin") tmapWithZE
    tmapWithEau <- TM.loadTexture renderer "assets/eau.bmp" (TextureId "eau") tmapWithAdmin


    -- Charger les textures pour les bâtiments améliorés
    tmapWithImprovedCabane <- TM.loadTexture renderer "assets/cabane_amelioree.bmp" (TextureId "cabane_amelioree") tmapWithEau
    tmapWithImprovedAtelier <- TM.loadTexture renderer "assets/atelier_amelioree.bmp" (TextureId "atelier_ameliore") tmapWithImprovedCabane
    tmapWithImprovedEpicerie <- TM.loadTexture renderer "assets/epicerie_amelioree.bmp" (TextureId "epicerie_amelioree") tmapWithImprovedAtelier

    return tmapWithImprovedEpicerie


-- Function to draw all buildings
drawBuildings :: Renderer -> TextureMap -> [Sim.Batiment] -> IO ()
drawBuildings renderer tmap buildings = mapM_ (drawBuilding renderer tmap) buildings

loadSprite :: Renderer -> FilePath -> SpriteId -> TextureMap -> IO (Sprite, TextureMap)
loadSprite renderer filePath spriteId textureMap = do
    updatedTextureMap <- TM.loadTexture renderer filePath (TextureId $ show spriteId) textureMap
    let sprite = createBuildingSprite (TextureId $ show spriteId) (S.mkArea 0 0 200 200)
    return (sprite, updatedTextureMap)

-- Fonction pour charger les sprites en utilisant les textures déjà chargées
-- Charger les sprites pour les bâtiments améliorés
loadSprites :: Renderer -> TextureMap -> SpriteMap -> IO SpriteMap
loadSprites renderer tmap smapInitial = do
    -- Charger les sprites pour différents types de bâtiments
    atelier <- createAndAddSprite renderer tmap (SpriteId "atelier")
    cabane <- createAndAddSprite renderer tmap (SpriteId "cabane")
    epicerie <- createAndAddSprite renderer tmap (SpriteId "epicerie")
    commissariat <- createAndAddSprite renderer tmap (SpriteId "commissariat")
    habitant <- createAndAddSprite renderer tmap (SpriteId "habitant")
    immigrant <- createAndAddSprite renderer tmap (SpriteId "immigrant")
    emigrant <- createAndAddSprite renderer tmap (SpriteId "emigrant")
    route <- createAndAddSprite renderer tmap (SpriteId "route")
    cable <- createAndAddSprite renderer tmap (SpriteId "cable")
    zr <- createAndAddSprite renderer tmap (SpriteId "ZR")
    zi <- createAndAddSprite renderer tmap (SpriteId "ZI")
    zc <- createAndAddSprite renderer tmap (SpriteId "ZC")
    ze <- createAndAddSprite renderer tmap (SpriteId "ZE")
    admin <- createAndAddSprite renderer tmap (SpriteId "admin")
    eau <- createAndAddSprite renderer tmap (SpriteId "eau")

    -- Créer la SpriteMap en ajoutant chaque sprite
    let updatedSmap = foldl' (\smap (id, sprite) -> SM.addSprite id sprite smap) smapInitial [atelier, cabane, epicerie, commissariat, immigrant, habitant, emigrant, route, cable, zr, zi, zc, ze, admin, eau]

    -- Créer et ajouter les sprites pour les bâtiments améliorés
    improvedCabane <- createAndAddSprite renderer tmap (SpriteId "cabane_amelioree")
    improvedAtelier <- createAndAddSprite renderer tmap (SpriteId "atelier_ameliore")
    improvedEpicerie <- createAndAddSprite renderer tmap (SpriteId "epicerie_amelioree")

    -- Ajouter les sprites des bâtiments améliorés
    let smapWithImproved = foldl' (\smap (id, sprite) -> SM.addSprite id sprite smap) updatedSmap [improvedCabane, improvedAtelier, improvedEpicerie]

    -- Créer et ajouter le sprite de background
    let backgroundSprite = createBuildingSprite (TextureId "background") (S.mkArea 0 0 800 800)
    let finalSmap = SM.addSprite (SpriteId "background") backgroundSprite smapWithImproved

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


createCitoyenSprite::TextureId -> S.Area -> Sprite
createCitoyenSprite textureId area =
    S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage textureId area


-- Function to determine the texture ID based on the building type
getTextureIdForBuilding :: Sim.Batiment -> TextureId
getTextureIdForBuilding (Sim.Cabane _ _ capacite _ _)
    | capacite > 10 = TextureId "cabane_amelioree"
    | otherwise = TextureId "cabane"
getTextureIdForBuilding (Sim.Atelier _ _ capacite _ _)
    | capacite > 10 = TextureId "atelier_ameliore"
    | otherwise = TextureId "atelier"
getTextureIdForBuilding (Sim.Epicerie _ _ capacite _ _)
    | capacite > 10 = TextureId "epicerie_amelioree"
    | otherwise = TextureId "epicerie"
getTextureIdForBuilding (Sim.Commissariat _ _ _) = TextureId "commissariat"

getTextureIdForZone :: Sim.Zone -> TextureId
getTextureIdForZone (Sim.Route _) = TextureId "route"
getTextureIdForZone (Sim.Cable _) = TextureId "cable"
getTextureIdForZone (Sim.ZR _ _) = TextureId "ZR"
getTextureIdForZone (Sim.ZI _ _) = TextureId "ZI"
getTextureIdForZone (Sim.ZC _ _) = TextureId "ZC"
getTextureIdForZone (Sim.ZE _) = TextureId "ZE"
getTextureIdForZone (Sim.Admin _ _) = TextureId "admin"
getTextureIdForZone (Sim.Eau _) = TextureId "eau"

getTextureIdForCitoyen:: Sim.Citoyen -> TextureId
getTextureIdForCitoyen (Sim.Habitant _ _ _ _) = TextureId "habitant"
getTextureIdForCitoyen (Sim.Immigrant _ _ _ ) = TextureId "immigrant"
getTextureIdForCitoyen (Sim.Emigrant _ _) = TextureId "emigrant"


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
    let argent = 100000

    -- Check if the city respects the property
    let villeRespectsProperty = Sim.prop_ville ville
    putStrLn $ "Does the city respect the property? " ++ show villeRespectsProperty

    -- Proceed with the game loop
    gameLoop 60 renderer tmap smap kbd ville font argent 0 (Sim.CitId 0) (Sim.BatId 0) Nothing  -- Pass the font to the game loop