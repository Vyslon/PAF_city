
module Mouse where

import SDL
import qualified SimCity as Sim
import qualified Data.Map as Map
import TextureMap
import Data.Maybe (isNothing)  -- Ajout de l'importation nécessaire
import Data.List (find)
import System.IO (hFlush, stdout)
import Data.List (foldl')


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


handleMouseEvents :: MyMouse -> Sim.Ville -> Renderer -> TextureMap -> IO Sim.Ville
handleMouseEvents mouse ville renderer tmap = do
    let mX = mouseX mouse
    let mY = mouseY mouse
    let maybeZone = findZone (Sim.getZones ville) mX mY  -- Assuming findZone is correctly defined elsewhere
    if mouseActif mouse && isNothing maybeZone then do
        newZone <- askForZoneDetails mX mY
        return $ Sim.addZone newZone ville
    else if mouseActif mouse then do
        newBuilding <- askForBuildingDetails mX mY
        let maybeZoneId = Sim.getZoneIdFromCoord (Sim.C mX mY) ville
        case maybeZoneId of
            Just zoneId -> return $ Sim.addBuildingToZone newBuilding zoneId ville
            Nothing -> do
                putStrLn "No valid zone found for the coordinates."
                return ville
    else
        return ville

findZone :: [Sim.Zone] -> Int -> Int -> Maybe Sim.Zone
findZone zones x y = find (zoneContainsPoint x y) zones

zoneContainsPoint :: Int -> Int -> Sim.Zone -> Bool
zoneContainsPoint x y zone = x >= minX && x <= maxX && y >= minY && y <= maxY
  where
    (minY, maxY, minX, maxX) = Sim.limites (Sim.zoneForme zone)



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

--test github bb