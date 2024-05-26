module Generators where

import Test.Hspec
import Test.QuickCheck
import Control.Monad (guard, foldM)
import System.Random (Random)
import Data.List (find)
import SimCity
import Data.Maybe
import qualified Data.Map as Map
import Data.IORef
import System.IO.Unsafe (unsafePerformIO)

--import Distribution.Compat.Prelude (undefined)


buildingCoord :: Batiment -> Coord
buildingCoord (Cabane _ coord _ _ _) = coord
buildingCoord (Atelier _ coord _ _ _) = coord
buildingCoord (Epicerie _ coord _ _ _) = coord
buildingCoord (Commissariat _ coord _) = coord

coordDepuisId :: Zone -> BatId -> Coord
coordDepuisId zone batId =
    let buildings = buildingsFromZone zone in
    let building = find (\b -> batIdFromBuilding b == batId) buildings
    in case building of
        Just b  -> buildingCoord b
        Nothing -> error "Building not found in the given zone (should not happen)"

-- Viens du TME8
samples :: Int -> Gen a -> IO [a]
samples n gen = do
  l <- sample' gen
  return $ take n l

genCoord :: Int -> Int -> Gen Coord
genCoord maxX maxY = C <$> choose (0, maxX) <*> choose (0, maxY)

-- Générateur de segment horizontal
genHSegment :: Int -> Int -> Int -> Gen Forme
genHSegment maxX maxY maxLongueur = do
    longueur <- choose (1, maxLongueur)
    coord <- genCoord (maxX - longueur) (maxY)
    return $ HSegment coord longueur

-- Générateur de segment vertical
genVSegment :: Int -> Int -> Int -> Gen Forme
genVSegment maxX maxY maxHauteur = do
    hauteur <- choose (1, maxHauteur)
    coord <- genCoord (maxX) (maxY - hauteur)
    return $ VSegment coord hauteur

-- Générateur de rectangle
genRectangle :: Int -> Int -> Int -> Int -> Gen Forme
genRectangle maxX maxY maxLargeur maxHauteur = do
    largeur <- choose (1, maxLargeur)
    hauteur <- choose (1, maxHauteur)
    coord <- genCoord (maxX - largeur) (maxY - hauteur)
    return $ Rectangle coord largeur hauteur

-- Générateur de forme
genForme :: Int -> Int -> Int -> Int -> Gen Forme
genForme maxX maxY maxLargeur maxHauteur = frequency [(30, genHSegment maxX maxY maxLargeur), (30, genVSegment maxX maxY maxHauteur), (40, genRectangle maxX maxY maxLargeur maxHauteur)]

genArgent :: Gen Int
genArgent = frequency [ (84, choose (500, 1000)),
                        (10, choose (1001, 10000)),
                        (5, choose (10001, 100000)),
                        (1, choose (100001, 1000000)) ] 

-- Générateur d'identifiant de citoyen
genCitId :: Gen CitId
genCitId = CitId <$> choose (1, 100000)

-- Générateur d'occupation
-- Entrée : 
--      * précise les coordonnées où le citoyen doit se rendre si on tombe sur l'occupation "se déplace vers"
--      * Id du batiment de l'entreprise dans lequel le citoyen travaille
--      * Id du batiment du magasin dans lequel le citoyen fais ses courses
genOccupation :: Maybe BatId -> Maybe BatId -> Coord -> Gen Occupation
genOccupation maybeEntreprise maybeMagasin coord = frequency [
    (if isJust maybeEntreprise then 25 else 0, pure Travaille), -- Travaille seulement si entreprise est différent de Nothing
    (25, pure Dors),
    (if isJust maybeMagasin then 25 else 0, pure FaisLesCourses), -- FaisLesCourses seulement si magasin est différent de Nothing
    (25, pure (SeDeplaceVers [coord])) -- TODO : remplacer par astar
    ]

{-# NOINLINE currentBatId #-}
currentBatId :: IORef Int
currentBatId = unsafePerformIO (newIORef 0)

-- Fonction pour obtenir un nouvel identifiant unique
getNextBatId :: IO BatId
getNextBatId = do
    batId <- readIORef currentBatId
    modifyIORef currentBatId (+1)
    return (BatId batId)

unsafeGetNextBatId :: BatId
unsafeGetNextBatId = unsafePerformIO getNextBatId

-- Générateur d'émigrant
-- Entrée : coordonnées de la sortie de la ville
genEmigrant :: Int -> Int -> Coord -> Gen Citoyen
genEmigrant maxX maxY sortieVille = do
    coord <- (genCoord maxX maxY)
    return $ Emigrant coord (SeDeplaceVers [sortieVille]) -- TODO : remplacer par astar

-- Générateur d'immigrant
-- Entrée : coordonnées du logement disponible le plus proche
genImmigrant :: Int -> Int -> Ville -> Coord -> Gen Citoyen
genImmigrant maxX maxY ville coordLogementDisponible = do
    coord <- (genCoord maxX maxY)
    argent <- genArgent
    fatigue <- choose (190, 250) -- Énergie max = 250 (12h d'activités) / 1h de sommeil (21 frames) => + 21 de fatigue
    faim <- choose (60, 80) -- Faim max = 80 (environ 4h sans avoir besoin de manger) / 1h de repas (21 frames) => faim à 80
    let path = aStar coord coordLogementDisponible ville
    return $ Immigrant coord (argent, fatigue, faim) (SeDeplaceVers (fromJust path))

-- Générateur de citoyen
-- Entrée :
--      * Id de la résidence dans laquelle le citoyen vit
--      * Id de l'entreprise dans laquelle le citoyen travaille
--      * Id du magasin dans lequel le citoyen fait ses courses
genHabitant :: Int -> Int -> Zone -> BatId -> Maybe Zone -> Maybe BatId -> Maybe Zone -> Maybe BatId -> Gen Citoyen
genHabitant maxX maxY zoneResidence residence zoneEntreprise entreprise zoneMagasin magasin = do
    -- Définir une coordonnée par défaut si les BatIds nécessaires sont absents
    let coordResidence = coordDepuisId zoneResidence residence
    occupation <- genOccupation entreprise magasin coordResidence
    argent <- genArgent
    fatigue <- choose (190, 250)
    faim <- choose (60, 80)
    coord <- case occupation of
        Travaille -> case (fromMaybe residence entreprise) of
            residence -> return $ coordDepuisId zoneResidence residence
            entreprise -> return $ coordDepuisId (fromJust zoneEntreprise) entreprise
        Dors -> return $ coordDepuisId zoneResidence residence
        FaisLesCourses ->  case (fromMaybe residence magasin) of
            residence -> return $ coordDepuisId zoneResidence residence
            magasin -> return $ coordDepuisId (fromJust zoneMagasin) magasin
        SeDeplaceVers _ -> genCoord maxX maxY
        _ -> return $ coordDepuisId zoneResidence residence
    return $ Habitant coord (argent, fatigue, faim) (residence, entreprise, magasin) occupation

genCitoyen :: Int -> Int -> Ville -> Coord -> Coord -> Zone -> BatId -> Maybe Zone -> Maybe BatId -> Maybe Zone -> Maybe BatId -> Gen Citoyen
genCitoyen maxX maxY ville sortieVille coordLogementDisponible zoneResidence residence zoneEntreprise entreprise zoneMagasin magasin =
    frequency [
        (20, genEmigrant maxX maxY sortieVille),
        (20, genImmigrant maxX maxY ville coordLogementDisponible),
        (60, genHabitant maxX maxY zoneResidence residence zoneEntreprise entreprise zoneMagasin magasin)
    ]

 
-- Fonction pour vérifier si une forme peut être placée à une coordonnée sans superposer un bâtiment existant
canPlaceForme :: Zone -> Forme -> Coord -> Bool
canPlaceForme zone forme coord =
    let movedForme = moveForme forme coord
        Rectangle (C zx zy) zw zh = zoneForme zone
    in all (\b -> not (collisionManuelle movedForme (getForme b))) (buildingsFromZone zone) &&
       withinZone movedForme (Rectangle (C zx zy) zw zh)

-- Fonction pour déplacer une forme à une nouvelle coordonnée
moveForme :: Forme -> Coord -> Forme
moveForme (Rectangle _ w h) (C x y) = Rectangle (C x y) w h
moveForme (HSegment _ l) (C x y) = HSegment (C x y) l
moveForme (VSegment _ l) (C x y) = VSegment (C x y) l

-- Fonction pour vérifier si une forme est bien dans les limites de la zone
withinZone :: Forme -> Forme -> Bool
withinZone forme (Rectangle (C zx zy) zw zh) =
    all (\(C x y) -> appartient (C x y) (Rectangle (C zx zy) zw zh)) (contient forme)


genCoordZone :: Zone -> Forme -> Gen (Maybe Coord)
genCoordZone zone formeBatiment = do
    if (aire (zoneForme zone) < aire formeBatiment) then do
        return Nothing
    else do
        let coords = contient (zoneForme zone)
        return $ find (canPlaceForme zone formeBatiment) coords

genCabane :: Int -> Int -> Zone -> Gen (Maybe Batiment)
genCabane maxX maxY zone = do
    forme <- genRectangle maxX maxY 50 50
    maybeCoord <- genCoordZone zone forme
    return $ fmap (\coord -> Cabane forme coord 0 [] unsafeGetNextBatId) maybeCoord

genAtelier :: Int -> Int -> Zone -> Gen (Maybe Batiment)
genAtelier maxX maxY zone = do
    forme <- genRectangle maxX maxY 50 50
    maybeCoord <- genCoordZone zone forme
    return $ fmap (\coord -> Atelier forme coord 0 [] unsafeGetNextBatId) maybeCoord

genEpicerie :: Int -> Int -> Zone -> Gen (Maybe Batiment)
genEpicerie maxX maxY zone = do
    forme <- genRectangle maxX maxY 50 50
    maybeCoord <- genCoordZone zone forme
    return $ fmap (\coord -> Epicerie forme coord 0 [] unsafeGetNextBatId) maybeCoord

genCommissariat :: Int -> Int -> Zone -> Gen (Maybe Batiment)
genCommissariat maxX maxY zone = do
    forme <- genRectangle maxX maxY 50 50
    maybeCoord <- genCoordZone zone forme
    return $ fmap (\coord -> Commissariat forme coord unsafeGetNextBatId) maybeCoord

genBatiment :: Int -> Int -> Zone -> Gen (Maybe Batiment)
genBatiment maxX maxY zone = case zone of
    (ZR forme batiments) -> genCabane maxX maxY (ZR forme batiments)
    (ZI forme batiments) -> genAtelier maxX maxY (ZI forme batiments)
    (ZC forme batiments) -> genEpicerie maxX maxY (ZC forme batiments)
    (Admin forme batiments) -> genCommissariat maxX maxY (Admin forme batiments)
    _ -> error "On essaie de générer un batiment dans une zone qui n'en possède pas"

-- TODO :  updateZoneWithBuilding :: Zone -> Batiment -> Zone

generateBuildings :: (Int -> Int -> Zone -> Gen (Maybe Batiment)) -> Int -> Int -> Zone -> Int -> Gen Zone
generateBuildings genBuilding maxX maxY zone num = foldM step zone [0..num-1]
    where step zoneCurr _  = do
            maybeBat <- (genBatiment maxX maxY zoneCurr)
            case maybeBat of
                Just bat -> return $ updateZoneWithBuilding bat zoneCurr 
                Nothing -> return zoneCurr

-- Générateur de zones TODO : si zone admin avec un commissariat aux coord (-1) : erreur
genZone :: Int -> Int -> Gen Zone
genZone maxX maxY = do
    zoneType <- elements ["ZR", "ZI", "ZC", "Admin"]
    forme <- genRectangle maxX maxY 100 100
    case zoneType of
        "ZR" -> do
            numBats <- choose (2, 5)  -- Nombre aléatoire de bâtiments entre 2 et 5
            newZone <- generateBuildings genCabane maxX maxY (ZR forme []) numBats
            return $ newZone
        "ZI" -> do
            numBats <- choose (2, 5)  -- Nombre aléatoire de bâtiments entre 2 et 5
            newZone <- generateBuildings genAtelier maxX maxY (ZI forme []) numBats
            return $ newZone
        "ZC" -> do
            numBats <- choose (2, 5)  -- Nombre aléatoire de bâtiments entre 2 et 5
            newZone <- generateBuildings genEpicerie maxX maxY (ZC forme []) numBats
            return $ newZone
        "Admin" -> do
            maybeBat1 <- genCommissariat maxX maxY (Admin forme (Commissariat (Rectangle (C (-1) (-1)) (-1) (-1)) (C (-1) (-1)) (BatId (-1))))
            return $ Admin forme (fromMaybe (Commissariat (Rectangle (C (-1) (-1)) (-1) (-1)) (C (-1) (-1)) (BatId (-1))) maybeBat1)
        _ -> error "Zone type not recognized"



-- Générer des zones sans chevauchement
generateZones :: Zone -> Int -> Gen [Zone]
generateZones initialZone num = foldM step [initialZone] [1..num-1]
  where
    step acc _ = do
        newZone <- genZone 640 480
        let currentVille = V (Map.fromList (zip (map ZoneId [0..]) acc)) Map.empty
        if collision2Zones newZone currentVille
            then return acc
            else return (newZone : acc)

-- Générateur de ville avec une cabane obligatoire
genVille :: Int -> Int -> Gen Ville
genVille maxX maxY = do
    initialCabane <- genCabane maxX maxY (ZR (Rectangle (C 0 0) maxX maxY) [])
    let initialZone = case initialCabane of
            Just cabane -> updateZoneWithBuilding cabane (ZR (Rectangle (C 0 0) maxX maxY) []) 
            Nothing -> ZR (Rectangle (C 0 0) maxX maxY) []  -- Fallback in case no cabane is generated
    zones <- generateZones initialZone 10  -- Adjust the number of zones as needed
    return $ V (Map.fromList (zip (map ZoneId [0..]) zones)) Map.empty

property_inv_genVille :: Property
property_inv_genVille = forAll (genVille 800 800) $ prop_ville

-- quickCheck prop_genBridgeOK_inv

genVilleOk = do
  describe "genVilleOk" $ do
    it "génère des villes qui satisfont leurs invariants" $
      property property_inv_genVille