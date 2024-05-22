
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



handleMouseEvents :: MyMouse -> Sim.Ville -> Int -> Renderer -> TextureMap -> IO (Sim.Ville, Int)
handleMouseEvents mouse ville argent renderer tmap = do
    let mX = mouseX mouse
    let mY = mouseY mouse
    let maybeZone = findZone (Sim.getZones ville) mX mY  -- Assuming findZone is correctly defined elsewhere
    let maybeBuilding = findBuilding maybeZone mX mY

    if mouseGaucheActif mouse && isNothing maybeZone then do
        newZone <- askForZoneDetails mX mY
        return $ (Sim.addZone newZone ville, argent)
    else if mouseGaucheActif mouse then do
        newBuilding <- askForBuildingDetails mX mY
        let maybeZoneId = Sim.getZoneIdFromCoord (Sim.C mX mY) ville
        case maybeZoneId of
            Just zoneId -> do
                (resville, resargent) <- pure $ (Sim.addBuildingToZone argent newBuilding zoneId ville)
                return $ (Sim.addBuildingToZone argent newBuilding zoneId ville)
                -- return $ Sim.addBuildingToZone argent newBuilding zoneId ville
            Nothing -> do
                putStrLn "No valid zone found for the coordinates."
                return (ville, argent)
    else if mouseDroiteActif mouse && isJust maybeZone then do
        if isJust maybeBuilding then do
            putStrLn "Suppression du batiment"
            let maybeZoneId = Sim.getZoneIdFromCoord (Sim.C mX mY) ville
            case maybeZoneId of
                Just zoneId -> do
                    return (updateZone zoneId (deleteBuilding (fromJust maybeBuilding) (fromJust maybeZone)) ville, argent)
                Nothing -> do
                    putStrLn "No valid zone found for the coordinates."
                    return (ville, argent)
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
                    return (Sim.V (Map.delete zoneId zones) cit, argent)
                Nothing -> do
                    putStrLn "No valid zone found for the coordinates."
                    return (ville, argent)
            
    else
        return (ville, argent)

updateZone :: Sim.ZoneId -> Sim.Zone -> Sim.Ville -> Sim.Ville
updateZone id zone (Sim.V zones cit) = Sim.V (Map.insert id zone zones) cit

findZone :: [Sim.Zone] -> Int -> Int -> Maybe Sim.Zone
findZone zones x y = find (zoneContainsPoint x y) zones

findBuilding :: Maybe Sim.Zone -> Int -> Int -> Maybe Sim.Batiment
findBuilding Nothing _ _ = Nothing
findBuilding (Just zone) mX mY = let buildings = (Sim.buildingsFromZone zone) in
    find (buildingContainsPoint mX mY) buildings

zoneContainsPoint :: Int -> Int -> Sim.Zone -> Bool
zoneContainsPoint x y zone = x >= minX && x <= maxX && y >= minY && y <= maxY
  where
    (minY, maxY, minX, maxX) = Sim.limites (Sim.zoneForme zone)

buildingContainsPoint :: Int -> Int -> Sim.Batiment -> Bool
buildingContainsPoint x y building = x >= minX && x <= maxX && y >= minY && y <= maxY
  where
    (minY, maxY, minX, maxX) = Sim.limites (Sim.getForme building)

askForZoneDetails :: Int -> Int -> IO Sim.Zone
askForZoneDetails x y = do
    putStrLn "\nChoisissez un type de zone:"
    putStrLn "1. Résidentielle (ZR)"
    putStrLn "2. Industrielle (ZI)"
    putStrLn "3. Commerciale (ZC)"
    putStrLn "4. Route"
    putStrLn "5. Eau"
    putStrLn "6. Administrative"
    putStr "Votre choix: "
    hFlush stdout
    choice <- getLine
    putStrLn "Entrez la largeur de la zone:"
    widthStr <- getLine
    putStrLn "Entrez la longueur de la zone:"
    heightStr <- getLine
    let width = read widthStr :: Int
    let height = read heightStr :: Int
    case choice of
        "1" -> return $ Sim.ZR (Sim.Rectangle (Sim.C x y) width height) []
        "2" -> return $ Sim.ZI (Sim.Rectangle (Sim.C x y) width height) []
        "3" -> return $ Sim.ZC (Sim.Rectangle (Sim.C x y) width height) []
        "4" -> return $ Sim.Route (Sim.Rectangle (Sim.C x y) width height)
        "5" -> return $ Sim.Eau (Sim.Rectangle (Sim.C x y) width height)
        "6" -> return $ Sim.Admin (Sim.Rectangle (Sim.C x y) width height) undefined -- Assurez-vous de définir le bâtiment administratif
        _ -> do
            putStrLn "Choix non valide, veuillez réessayer."
            askForZoneDetails x y

askForBuildingDetails :: Int -> Int -> IO Sim.Batiment
askForBuildingDetails x y = do
    putStrLn "\nChoisissez un type de bâtiment:"
    putStrLn "0. Supprimer la zone"
    putStrLn "1. Cabane"
    putStrLn "2. Atelier"
    putStrLn "3. Épicerie"
    putStrLn "4. Commissariat"
    putStr "Votre choix: "
    hFlush stdout
    choice <- getLine
    putStrLn "Entrez la largeur du bâtiment:"
    widthStr <- getLine
    putStrLn "Entrez la longueur du bâtiment:"
    heightStr <- getLine
    let width = read widthStr :: Int
    let height = read heightStr :: Int
    let coord = Sim.C x y
    case choice of
        "1" -> return $ Sim.Cabane (Sim.Rectangle coord width height) coord 5 [] (Sim.BatId 1 ) -- Assumons une capacité fixe pour simplifier
        "2" -> return $ Sim.Atelier (Sim.Rectangle coord width height) coord 5 [] (Sim.BatId 2)
        "3" -> return $ Sim.Epicerie (Sim.Rectangle coord width height) coord 5 [] (Sim.BatId 3)
        "4" -> return $ Sim.Commissariat (Sim.Rectangle coord width height) coord ( Sim.BatId 4)
        _ -> do
            putStrLn "Choix non valide, veuillez réessayer."
            askForBuildingDetails x y