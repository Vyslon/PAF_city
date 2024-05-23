
module Mouse where

import SDL
import qualified SimCity as Sim
import qualified Data.Map as Map
import TextureMap
import Data.Maybe (isNothing, isJust, fromJust)  -- Ajout de l'importation nécessaire
import Data.List (find)
import System.IO (hFlush, stdout)
import Data.List (foldl')


data MyMouse = MyMouse { gaucheActif :: Bool, droiteActif :: Bool, mouseX :: Int
                   , mouseY :: Int }

-- ((mouseButtonEventButton mbep) == ButtonLeft) && 

handleEventMousePos :: Event -> MyMouse -> MyMouse 
handleEventMousePos event _ =
  case eventPayload event of
    MouseButtonEvent mbep ->
        if (mouseButtonEventButton mbep) == ButtonLeft then
            if mouseButtonEventMotion mbep == Pressed
                then  let (P (V2 x y)) = mouseButtonEventPos mbep in MyMouse True False (fromIntegral x) (fromIntegral y)
            else -- click released
                let (P (V2 x y)) = mouseButtonEventPos mbep in MyMouse False False (fromIntegral x) (fromIntegral y)
        else if (mouseButtonEventButton mbep) == ButtonRight then
            if mouseButtonEventMotion mbep == Pressed
                then  let (P (V2 x y)) = mouseButtonEventPos mbep in MyMouse False True (fromIntegral x) (fromIntegral y)
            else -- click released
                let (P (V2 x y)) = mouseButtonEventPos mbep in MyMouse False False (fromIntegral x) (fromIntegral y)
        else MyMouse False False (-1) (-1)
    _ -> MyMouse False False (-1) (-1)

handleEventsMousePos :: [Event] -> MyMouse -> MyMouse
handleEventsMousePos events mse = foldl' (flip handleEventMousePos) mse events

mouseGaucheActif :: MyMouse -> Bool
mouseGaucheActif (MyMouse res _ _ _) = res

mouseDroiteActif :: MyMouse -> Bool
mouseDroiteActif (MyMouse _ res _ _) = res

deleteBuildingFromList :: Sim.Batiment -> [Sim.Batiment] -> [Sim.Batiment]
deleteBuildingFromList _ [] = []
deleteBuildingFromList build (building:xs)
    | building == build = xs
    | otherwise = building:(deleteBuildingFromList build xs)

deleteBuilding :: Sim.Batiment -> Sim.Zone -> Sim.Zone
deleteBuilding _ (Sim.Admin x xs) = Sim.Admin x xs
deleteBuilding build zone = let buildings = (Sim.buildingsFromZone zone) in
    let newBuildings = (deleteBuildingFromList build buildings) in
    Sim.updateBuildingsFromZone newBuildings zone



handleMouseEvents :: MyMouse -> Sim.Ville -> Int -> Renderer -> TextureMap -> Sim.CitId -> Sim.BatId -> IO (Sim.Ville, Sim.BatId, Int)
handleMouseEvents mouse ville argent renderer tmap citId batId = do
    let mX = mouseX mouse
    let mY = mouseY mouse
    let maybeZone = findZone (Sim.getZones ville) mX mY  -- Assuming findZone is correctly defined elsewhere

    if mouseGaucheActif mouse then do
        case maybeZone of
            Just zone -> do
                let maybeBuilding = findBuildingInZone (mX, mY) zone
                case maybeBuilding of
                    Just building -> do
                        newVille <- handleBuildingClick building ville
                        return (newVille, batId, argent)
                    Nothing -> do
                        putStrLn "No building found at these coordinates."
                        maybeNewBuilding <- askForBuildingDetails mX mY batId
                        case maybeNewBuilding of
                            Just newBuilding -> do
                                let newBatId = Sim.incrementBatId batId
                                let maybeZoneId = Sim.getZoneIdFromCoord (Sim.C mX mY) ville
                                case maybeZoneId of
                                    Just zoneId -> do
                                        let (updatedVille, newArgent) = Sim.addBuildingToZone argent newBuilding zoneId ville
                                        return (updatedVille, newBatId, newArgent)
                                    Nothing -> do
                                        putStrLn "Error: Zone ID not found."
                                        return (ville, batId, argent)
                            Nothing -> return (ville, batId, argent)
            Nothing -> do
                putStrLn "No zone found at these coordinates. Would you like to create a new zone here? (yes/no)"
                decision <- getLine
                if decision == "yes" then do
                    maybeNewZone <- askForZoneDetails mX mY
                    case maybeNewZone of
                        Just newZone -> do
                            let newVille = Sim.addZone newZone ville
                            return (newVille, batId, argent)
                        Nothing -> return (ville, batId, argent)
                else
                    return (ville, batId, argent)
    else if mouseDroiteActif mouse && isJust maybeZone then do
        let maybeBuilding = findBuildingInZone (mX, mY) (fromJust maybeZone)
        if isJust maybeBuilding then do
            putStrLn "Suppression du batiment"
            let maybeZoneId = Sim.getZoneIdFromCoord (Sim.C mX mY) ville
            case maybeZoneId of
                Just zoneId -> do
                    return (updateZone zoneId (deleteBuilding (fromJust maybeBuilding) (fromJust maybeZone)) ville, batId, argent)
                Nothing -> do
                    putStrLn "No valid zone found for the coordinates."
                    return (ville, batId, argent)
        else
            do
            putStrLn "Suppression de la zone"
            let maybeZoneId = Sim.getZoneIdFromCoord (Sim.C mX mY) ville
            case maybeZoneId of
                Just zoneId -> do
                    putStrLn "Okay"
                    let zones = Sim.viZones ville
                    putStrLn (show (Map.size zones))
                    let cit = Sim.viCit ville
                    putStrLn (show (Map.size (Map.delete zoneId zones)))
                    return (Sim.V (Map.delete zoneId zones) cit, batId, argent)
                Nothing -> do
                    putStrLn "No valid zone found for the coordinates."
                    return (ville, batId, argent)
            
    else
        return (ville, batId, argent)

updateZone :: Sim.ZoneId -> Sim.Zone -> Sim.Ville -> Sim.Ville
updateZone id zone (Sim.V zones cit) = Sim.V (Map.insert id zone zones) cit

findZone :: [Sim.Zone] -> Int -> Int -> Maybe Sim.Zone
findZone zones x y = find (zoneContainsPoint x y) zones

zoneContainsPoint :: Int -> Int -> Sim.Zone -> Bool
zoneContainsPoint x y zone = x >= minX && x <= maxX && y >= minY && y <= maxY
  where
    (minY, maxY, minX, maxX) = Sim.limites (Sim.zoneForme zone)

buildingContainsPoint :: Int -> Int -> Sim.Batiment -> Bool
buildingContainsPoint x y building = x >= minX && x <= maxX && y >= minY && y <= maxY
  where
    (minY, maxY, minX, maxX) = Sim.limites (Sim.getForme building)


askForZoneDetails :: Int -> Int -> IO (Maybe Sim.Zone)
askForZoneDetails x y = do
    putStrLn "\nChoisissez un type de zone:"
    putStrLn "1. Résidentielle (ZR)"
    putStrLn "2. Industrielle (ZI)"
    putStrLn "3. Commerciale (ZC)"
    putStrLn "4. Route"
    putStrLn "5. Eau"
    putStrLn "6. Administrative"
    putStrLn "7. Quitter ce menu"
    putStr "Votre choix: "
    hFlush stdout
    choice <- getLine

    case choice of
        "7" -> do
            putStrLn "Quitter le menu de sélection des zones."
            return Nothing
        _ -> if choice `elem` ["1", "2", "3", "4", "5", "6"] then do
                putStrLn "Entrez la largeur de la zone:"
                widthStr <- getLine
                putStrLn "Entrez la longueur de la zone:"
                heightStr <- getLine
                let width = read widthStr :: Int
                let height = read heightStr :: Int
                let coord = Sim.C x y
                return $ Just $ case choice of
                    "1" -> Sim.ZR (Sim.Rectangle coord width height) []
                    "2" -> Sim.ZI (Sim.Rectangle coord width height) []
                    "3" -> Sim.ZC (Sim.Rectangle coord width height) []
                    "4" -> Sim.Route (Sim.Rectangle coord width height)
                    "5" -> Sim.Eau (Sim.Rectangle coord width height)
                    "6" -> Sim.Admin (Sim.Rectangle coord width height) undefined -- Assurez-vous de définir le bâtiment administratif correctement
            else do
                putStrLn "Choix non valide, veuillez réessayer."
                askForZoneDetails x y


askForBuildingDetails :: Int -> Int -> Sim.BatId -> IO (Maybe Sim.Batiment)
askForBuildingDetails x y bid = do
    putStrLn "\nChoisissez un type de bâtiment:"
    putStrLn "1. Cabane"
    putStrLn "2. Atelier"
    putStrLn "3. Épicerie"
    putStrLn "4. Commissariat"
    putStrLn "5. Quitter ce menu"
    putStr "Votre choix: "
    hFlush stdout
    choice <- getLine

    case choice of
        "5" -> do
            putStrLn "Quitter le menu de sélection des bâtiments."
            return Nothing
        _ -> if choice `elem` ["1", "2", "3", "4"] then do
                putStrLn "Entrez la largeur du bâtiment:"
                widthStr <- getLine
                putStrLn "Entrez la longueur du bâtiment:"
                heightStr <- getLine
                let width = read widthStr :: Int
                let height = read heightStr :: Int
                let coord = Sim.C x y
                return $ Just $ case choice of
                    "1" -> Sim.Cabane (Sim.Rectangle coord width height) coord 5 [] bid
                    "2" -> Sim.Atelier (Sim.Rectangle coord width height) coord 5 [] bid
                    "3" -> Sim.Epicerie (Sim.Rectangle coord width height) coord 5 [] bid
                    "4" -> Sim.Commissariat (Sim.Rectangle coord width height) coord bid
            else do
                putStrLn "Choix non valide, veuillez réessayer."
                askForBuildingDetails x y bid

findBuildingInZone :: (Int, Int) -> Sim.Zone -> Maybe Sim.Batiment
findBuildingInZone (x, y) zone = 
    case zone of
        Sim.ZR _ buildings -> find (buildingContainsPoint x y) buildings
        Sim.ZI _ buildings -> find (buildingContainsPoint x y) buildings
        Sim.ZC _ buildings -> find (buildingContainsPoint x y) buildings
        _ -> Nothing

-- Gère les clics sur les bâtiments en proposant des actions spécifiques
handleBuildingClick :: Sim.Batiment -> Sim.Ville -> IO Sim.Ville
handleBuildingClick building ville = do
    case building of
        Sim.Cabane _ _ _ _ batId -> do
            putStrLn "Options: [1] Installer un nouvel habitant"
            choice <- getLine
            case choice of
                "1" -> do
                    let maybeImmigrantId = Sim.findFirstImmigrantId ville
                    case maybeImmigrantId of
                        Just immigrantId -> case Sim.addImmigrantToCabane immigrantId batId ville of
                            Just updatedVille -> do
                                putStrLn "Nouvel habitant ajouté avec succès."
                                return updatedVille
                            Nothing -> do
                                putStrLn "Échec de l'ajout de l'habitant. Cabane pleine ou problème de CitId."
                                return ville
                        Nothing -> do
                            putStrLn "Aucun immigrant disponible pour l'installation."
                            return ville
                _ -> return ville
        _ -> do
            putStrLn "Aucune action spéciale disponible pour ce type de bâtiment."
            return ville