module SimCity where

import qualified Data.Map as Map
import Data.List (find)

data Coord = C {cx :: Int, cy :: Int} deriving (Show , Eq)

data Forme = HSegment Coord Int
    | VSegment Coord Int
    | Rectangle Coord Int Int
-- on devrait pas faire le cas Segment en vrai, c'est juste un rectangle de largeur 1
-- qui va complexifier la suite

instance Eq Forme where
    (HSegment (C x1 y1) n1) == (HSegment (C x2 y2) n2) = x1 == x2 && y1 == y2 && n1 == n2
    (VSegment (C x1 y1) n1) == (VSegment (C x2 y2) n2) = x1 == x2 && y1 == y2 && n1 == n2
    (Rectangle (C x1 y1) n1 p1) == (Rectangle (C x2 y2) n2 p2) = x1 == x2 && y1 == y2 && n1 == n2 && p1 == p2
    _ == _ = False

instance Show Forme where
    show (HSegment (C x y) length) = "HSegment (C " ++ show x ++ " " ++ show y ++ ") " ++ show length
    show (VSegment (C x y) height) = "VSegment (C " ++ show x ++ " " ++ show y ++ ") " ++ show height
    show (Rectangle (C x y) width height) = "Rectangle (C " ++ show x ++ " " ++ show y ++ ") " ++ show width ++ " " ++ show height


-- (nord, sud, ouest, est)
limites::Forme -> (Int, Int, Int, Int)
limites (HSegment (C x y) longueur) = (y, y, x, x + longueur)
limites (VSegment (C x y) hauteur) = (y, y - hauteur, x, x)
limites (Rectangle (C x y) width height) = (y, y + height, x, x + width)

appartient::Coord -> Forme -> Bool
appartient (C x1 y1) (HSegment (C x2 y2) longueur) =
    y1 == y2 && x1 >= x2 && x1 <= x2 + longueur
appartient (C x1 y1) (VSegment (C x2 y2) hauteur) =
    x1 == x2 && y1 >= y2 && y1 <= y2 + hauteur
appartient (C x1 y1) (Rectangle (C x2 y2) largeur hauteur) =
    x1 >= x2 && x1 <= x2 + largeur && y1 >= y2 && y1 <= y2 + hauteur


adjacent::Coord -> Forme -> Bool
adjacent (C x1 y1) (HSegment (C x2 y2) longueur) 
    | y1 == y2  = (abs (x1 - x2) <= 30) || (abs (x1 - (x2 + longueur)) <= 30)
    | otherwise = (x1 >= (x2 - 30) && x1 <= (x2 + longueur + 30)) && ((abs (y1 - y2) <= 30))

adjacent (C x1 y1) (VSegment (C x2 y2) hauteur)
    | x1 == x2 = (abs (y1 - y2) <= 30) || (abs (y1 - (y2 - hauteur)) <= 30)
    | otherwise = (y1 >= (y2 - hauteur - 30) && y1 <= (y2 + 30)) && ((abs (x1 - x2) <= 30))

adjacent (C x1 y1) (Rectangle (C x2 y2) largeur hauteur)
    | abs (x1 - (x2 - 30)) <= 30 || abs (x1 - (x2 + largeur + 30)) <= 30 = (y1 >= (y2 - 30) && y1 <= (y2 + hauteur + 30))
    | abs (y1 - (y2 - 30)) <= 30 || abs (y1 - (y2 + hauteur + 30)) <= 30 = (x1 >= (x2 - 30) && x1 <= (x2 + largeur + 30))
    | otherwise = False

contient::Forme -> [Coord]
contient (HSegment (C x y) longueur)
    | longueur > 0 = (C x y):(contient (HSegment (C (x + 1) y) (longueur - 1)))
    | longueur < 0 = []
    | otherwise = [(C x y)]
contient (VSegment (C x y) hauteur)
    | hauteur > 0 = (C x y):(contient (VSegment (C x (y - 1)) (hauteur - 1)))
    | hauteur < 0 = []
    | otherwise = [(C x y)]
contient (Rectangle (C x y) largeur hauteur)
    | hauteur == 0 = contient (HSegment (C x y) largeur)
    | largeur == 0 = contient (VSegment (C x y) hauteur)
    | (hauteur < 0) || (largeur < 0) = []
    | otherwise = (contient (HSegment (C x y) largeur))++(contient (VSegment (C x (y - 1)) (hauteur - 2)))++(contient (HSegment (C x (y - hauteur)) largeur))++(contient (VSegment (C (x + largeur) (y - 1)) (hauteur - 2)))


--Collision exacte:
collisionManuelle :: Forme -> Forme -> Bool
collisionManuelle forme1 forme2 = 
    let coords1 = contient forme1
        coords2 = contient forme2
    in any (`elem` coords2) coords1

-- double foldr avec adjacent (quasiment pareil que collision_approx)
adjacentes :: Forme -> Forme -> Bool
adjacentes forme1 forme2 = nonChevauchement && (horizontalementAdjacente || verticalementAdjacente)
  where
    (nord1, sud1, ouest1, est1) = limites forme1
    (nord2, sud2, ouest2, est2) = limites forme2

    -- Vérifie qu'il n'y a pas de chevauchement
    nonChevauchement = not (collisionManuelle forme1 forme2)

    -- Vérifie l'adjacence horizontale
    horizontalementAdjacente =
      (est1 + 1 == ouest2 || ouest1 - 1 == est2) && (nord1 <= nord2 && sud1 >= sud2 || nord2 <= nord1 && sud2 >= sud1)

    -- Vérifie l'adjacence verticale
    verticalementAdjacente =
      (sud1 - 1 == nord2 || nord1 + 1 == sud2) && (ouest1 <= ouest2 && est1 >= est2 || ouest2 <= ouest1 && est2 >= est1)

-- à verifier


newtype ZoneId = ZoneId Int deriving (Eq, Ord)
newtype BatId = BatId Int deriving (Eq, Show)
newtype CitId = CitId String deriving (Eq, Ord)

data Batiment = Cabane Forme Coord Int [CitId] BatId
    | Atelier Forme Coord Int [CitId] BatId
    | Epicerie Forme Coord Int [CitId] BatId
    | Commissariat Forme Coord BatId

data Zone = Eau Forme
    | Route Forme
    | ZR Forme [Batiment]
    | ZI Forme [Batiment] 
    | ZC Forme [Batiment] 
    | Admin Forme Batiment

instance Eq Zone where -- TODO : vérifir que la liste batiment est la même, créer une fonction pour ça
    (Eau f1) == (Eau f2) = f1 == f2
    (Route f1) == (Route f2) = f1 == f2
    (ZR f1 _) == (ZR f2 _) = f1 == f2
    (ZI f1 _) == (ZI f2 _) = f1 == f2
    (ZC f1 _) == (ZC f2 _) = f1 == f2
    (Admin f1 _) == (Admin f2 _) = f1 == f2
    _ == _ = False

data Occupation = Travaille
  | Chomage
  | Dors
  | FaisLesCourses
  | SeDeplaceVers Coord
  deriving (Show, Eq)

data Citoyen = Immigrant Coord (Int, Int, Int) Occupation
    | Habitant Coord (Int, Int, Int) (BatId, Maybe BatId, Maybe BatId) Occupation 
    | Emigrant Coord Occupation

zoneForme::Zone -> Forme
zoneForme (Eau forme) = forme
zoneForme (Route forme) = forme
zoneForme (ZR forme _) = forme
zoneForme (ZI forme _) = forme
zoneForme (ZC forme _) = forme
zoneForme (Admin forme _) = forme

data Ville = V { viZones :: Map.Map ZoneId Zone, viCit :: Map.Map CitId Citoyen }

-- Consigne : Dans les questions suivantes, on ne fera plus de supposition sur les constructeurs de Forme
-- (par exemple, on n’´ecrira plus HSegment), on utilisera uniquement les trois fonctions pr´ec´edentes. (Ainsi,
-- si on ajoute de nouveaux constructeurs `a Forme, seules les trois fonctions pr´ec´edentes devront ˆetre mises `a
-- jour).

-- TODO : Question 1.3
-- TODO : 3 invariants, 1 pour chaque condition

-- Fonction qui vérifie qu'une zone n'est en collision avec aucune autre zone dans une ville
collision2Zones :: Zone -> Ville -> Bool
collision2Zones zone ville = Map.foldr step True (viZones ville)
  where
    step currentZone acc = acc && (currentZone == zone || not (collisionManuelle (zoneForme zone) (zoneForme currentZone)))


-- Propriété qui vérifie que toutes les zones dans une ville ne sont pas en collision les unes avec les autres
prop_ville_sansCollision :: Ville -> Bool
prop_ville_sansCollision ville = Map.foldr step True (viZones ville)
  where
    step zone acc = acc && collision2Zones zone ville
        
-- TODO : prop_ville_sansCollision v = prop_zones_disjointes v && prop_ZRCI_adjacent_route v && routes_connexes v




-- TODO : Question 1.4 : Ecrire un invariant pour Ville.

verifieRoute ::Zone -> Bool
verifieRoute (Route _) = True
verifieRoute _ = False


verifieAdjacenceAuneRoute::Zone -> Ville -> Bool
verifieAdjacenceAuneRoute zone ville =
    Map.foldr step False (viZones ville)
    where
        step zoneCurrent acc =  acc || ((verifieRoute zoneCurrent) && (adjacentes (zoneForme zoneCurrent) (zoneForme zone)))
-- TODO : tous les citoyens ont une résidence ?

prop_verifieAllZonesAdjacentesRoute :: Ville -> Bool
prop_verifieAllZonesAdjacentesRoute ville =
    Map.foldr step True (viZones ville )
    where
        step zone@(Route forme) acc = True --si on est sur une route, c'est vrai, pas besoin d'adjacence
        step zone acc =  acc && (verifieAdjacenceAuneRoute zone ville) -- sinon need adjacence



prop_ville :: Ville -> Bool
prop_ville ville = prop_ville_sansCollision ville && prop_verifieAllZonesAdjacentesRoute ville 

construit::Ville -> Zone -> Ville
construit (V zones cit) z = (V (Map.insert (ZoneId (Map.size zones)) z zones) cit)

-- TODO : Question 1.6

pre_construit :: Ville -> Zone -> Bool
pre_construit ville zone =
    let newZoneId = ZoneId $ Map.size (viZones ville) + 1  in
    let mapAfter = Map.insert newZoneId zone (viZones ville) in 
    let ville2 = V mapAfter (viCit ville)  
    in prop_verifieAllZonesAdjacentesRoute ville2 && prop_ville_sansCollision ville2



post_construit::Ville -> Zone -> Ville -> Bool
post_construit villeAvant zone villeApres = (zonePresente (viZones villeApres) zone) && (Map.size (viZones villeApres) == Map.size (viZones villeAvant) + 1)

zonePresente::Map.Map ZoneId Zone -> Zone -> Bool
zonePresente map zoneATrouver =  any (== zoneATrouver) (Map.elems map)

-- TODO : ER1 à partir de la Question 1.8


getEntry :: Batiment -> Coord
getEntry (Cabane _ c _ _  _) = c
getEntry (Atelier _ c _ _ _ ) = c
getEntry (Epicerie _ c _ _  _) = c
getEntry (Commissariat _ c _ )= c 

getForme :: Batiment -> Forme
getForme (Cabane forme _ _ _  _ ) = forme
getForme (Atelier forme _ _ _ _  ) = forme
getForme (Epicerie forme _ _ _ _  ) = forme
getForme (Commissariat forme _ _ )= forme

prop_verifyEntry::Batiment -> Bool
prop_verifyEntry batiment = adjacent (getEntry batiment) (getForme batiment) 




getOccupants::Batiment -> Int 
getOccupants (Cabane _ _ n _ _ ) = n 
getOccupants (Atelier _ _ n _ _) = n 
getOccupants (Epicerie _ _ n _ _) = n 
getOccupants (Commissariat _ _ _) = 0


verifyIntLessThanListLength :: Batiment -> Bool
verifyIntLessThanListLength (Cabane _ _ n citIds _) = n <= length citIds
verifyIntLessThanListLength (Atelier _ _ n citIds _) = n <= length citIds
verifyIntLessThanListLength (Epicerie _ _ n citIds _) = n <= length citIds
verifyIntLessThanListLength (Commissariat _ _ _) = True


sous_fonction_entry_appart_route::Coord -> Ville -> Bool
sous_fonction_entry_appart_route c ville = 
    Map.foldr step False (viZones ville)
    where
        step zone acc = acc || ((verifieRoute zone) && (appartient c  (zoneForme zone) ))


-- Extract buildings from a Zone
buildingsFromZone :: Zone -> [Batiment]
buildingsFromZone (ZR _ bldgs) = bldgs
buildingsFromZone (ZI _ bldgs) = bldgs
buildingsFromZone (ZC _ bldgs) = bldgs
buildingsFromZone (Admin _ bldg) = [bldg]
buildingsFromZone _ = []  -- Eau and Route have no buildings

--extraire tous les batiments d'une ville
getAllBuildings :: Ville -> [Batiment]
getAllBuildings ville = concatMap buildingsFromZone (Map.elems (viZones ville))

prop_entry_appartient_route :: Ville -> Bool
prop_entry_appartient_route ville = all (\bldg -> sous_fonction_entry_appart_route (getEntry bldg) ville) (getAllBuildings ville)


--Verifier que le batiment appartient a la bonne zone, bon en sah clc mais possible
buildingInCorrectZone :: Batiment -> Zone -> Bool
buildingInCorrectZone (Cabane _ _ _ _ _) (ZR _ _ )  = True
buildingInCorrectZone (Atelier _ _ _ _ _) (ZI _ _ ) = True
buildingInCorrectZone (Epicerie _ _ _ _ _) (ZC _ _ ) = True
buildingInCorrectZone (Commissariat _ _ _) (Admin _ _ ) = True  -- Commissariats can be in any zone
buildingInCorrectZone _ _ = False  -- Default case if none match

-- Iterate over each zone in the city, and check every building in those zones
prop_zoningLaws :: Ville -> Bool
prop_zoningLaws (V vizones _) =
  all checkZoneBuildings (Map.elems vizones )
  where
    checkZoneBuildings :: Zone -> Bool
    checkZoneBuildings (ZR _ bats) = all (`buildingInCorrectZone` (ZR undefined bats)) bats
    checkZoneBuildings (ZI _ bats) = all (`buildingInCorrectZone` (ZI undefined bats)) bats
    checkZoneBuildings (ZC _ bats) = all (`buildingInCorrectZone` (ZC undefined bats)) bats
    checkZoneBuildings (Admin _ bat) = buildingInCorrectZone bat (Admin undefined bat)
    checkZoneBuildings _ = True  -- Eau and Route do not contain buildings


{-Version nulle avec trop de pattern matching
prop_batiments_in_Zone::Zone->Bool
prop_batiments_in_Zone (Eau _) = True
prop_batiments_in_Zone (Route _ ) = True
prop_batiments_in_Zone (ZR forme bats) = 
    foldr step True bats
    where
        step acc bat = 
            let (x1,x2,y1,y2) = limites bat in
                acc && (appartient x1 forme) && (appartient x2 forme) && (appartient y1 forme) && (appartient y2 forme)


-}

-- Helper function to check if a building is within a given Forme
buildingWithinZone :: Batiment -> Forme -> Bool
buildingWithinZone batiment zoneForme =
    let (nord, sud, ouest, est) = limites (getForme batiment)
        coinNordOuest = C ouest nord
        coinNordEst = C est nord
        coinSudOuest = C ouest sud
        coinSudEst = C est sud
    in all (\coin -> appartient coin zoneForme) [coinNordOuest, coinNordEst, coinSudOuest, coinSudEst]

-- Generic property to verify buildings are within their respective zones
prop_batiments_in_Zone :: Zone -> Bool
prop_batiments_in_Zone (Eau _) = True
prop_batiments_in_Zone (Route _) = True
prop_batiments_in_Zone (ZR forme bats) = all (`buildingWithinZone` forme) bats
prop_batiments_in_Zone (ZI forme bats) = all (`buildingWithinZone` forme) bats
prop_batiments_in_Zone (ZC forme bats) = all (`buildingWithinZone` forme) bats
prop_batiments_in_Zone (Admin forme bat) = buildingWithinZone bat forme




-- Faire demenagement d'un habitant,faire prop pour vérifier que l'habitant habite dans tel immeuble,
-- qu'il travaille dans tel truc, qu'il fait bien ses courses dans tel truc...


changerOccupation::Citoyen->Occupation->Citoyen
changerOccupation (Immigrant a b occ) new_occupation = Immigrant a b new_occupation
changerOccupation (Habitant a b c occ) new_occupation = Habitant a b c new_occupation
changerOccupation (Emigrant a occ) new_occupation = Emigrant a new_occupation


getOccupation::Citoyen ->Occupation
getOccupation (Immigrant _ _ occ)  = occ
getOccupation (Habitant _ _ _  occ)  = occ
getOccupation (Emigrant _ occ)  =occ


recupererChomage::Ville->Int
recupererChomage ville = 
    Map.foldr step 0 (viCit ville) 
    where 
        step cit acc  = if getOccupation cit == Chomage then acc+1 else acc





-- Function to add a new zone to a city
addZone :: Zone -> Ville -> Ville
addZone zone ville = 
    if pre_construit ville zone then
        let villeApres = construit  ville zone-- Suppose que Sim.addZone ajoute simplement la zone sans autres vérifications
        in if post_construit ville zone villeApres then
            villeApres  -- Retourne la ville modifiée si la postcondition est validée
        else ville  -- Retourne la ville originale si la postcondition échoue
    else ville  -- Retourne la ville originale si la précondition échoue

-- Function to add a new building to a zone
addBuildingToZone :: Batiment -> ZoneId -> Ville -> Ville
addBuildingToZone newBuilding zoneId ville = 
    case Map.lookup zoneId (viZones ville) of
        Just zone ->
            if buildingInCorrectZone newBuilding zone then
                let updatedZone = updateZoneWithBuilding zone newBuilding
                in ville { viZones = Map.insert zoneId updatedZone (viZones ville) }
            else
                ville
        Nothing -> error "Zone not found"

-- Helper function to add a building to the appropriate zone type
updateZoneWithBuilding :: Zone -> Batiment -> Zone
updateZoneWithBuilding (ZR forme bats) newBuilding = ZR forme (newBuilding : bats)
updateZoneWithBuilding (ZI forme bats) newBuilding = ZI forme (newBuilding : bats)
updateZoneWithBuilding (ZC forme bats) newBuilding = ZC forme (newBuilding : bats)
updateZoneWithBuilding other _ = other  -- Handle other cases or errors gracefully



getZones ::Ville ->[Zone]
getZones (V vizones _) = 
    Map.foldr step [] vizones 
    where
        step zone acc = acc ++ [zone]


{-
attribuerLogement :: Citoyen -> BatId -> Ville -> Ville
attribuerLogement citoyen batId ville = case Map.lookup batId ville of
    Just (Cabane forme coord capacite citIds) ->
        if length citIds < capacite
        then Map.insert batId (Cabane forme coord capacite (citoyenId citoyen : citIds)) ville
        else ville
    Just (Atelier forme coord capacite citIds) ->
        if length citIds < capacite
        then Map.insert batId (Atelier forme coord capacite (citoyenId citoyen : citIds)) ville
        else ville
    Just (Epicerie forme coord capacite citIds) ->
        if length citIds < capacite
        then Map.insert batId (Epicerie forme coord capacite (citoyenId citoyen : citIds)) ville
        else ville
    Just (Commissariat _ _) ->
        error "Commissariats cannot house citizens"
    Nothing ->
        error "Batiment not found"

        -}
-- TODO : tous les citoyens ont une résidence ?


createInitialVille :: Ville
createInitialVille = V {
    viZones = Map.empty,
    viCit = Map.empty  -- Assuming there are no citizens initially or define some if needed
}


getZoneIdFromCoord :: Coord -> Ville -> Maybe ZoneId
getZoneIdFromCoord coord ville = Map.foldrWithKey findZoneId Nothing (viZones ville)
  where
    findZoneId :: ZoneId -> Zone -> Maybe ZoneId -> Maybe ZoneId
    findZoneId zid zone acc = if appartient coord (zoneForme zone)
                              then Just zid 
                              else acc

sommeArgentCitoyens :: Ville -> Int
sommeArgentCitoyens ville = sum [argent | citoyen <- Map.elems (viCit ville), let argent= getCitoyenMoney citoyen]
  where
    getCitoyenMoney (Immigrant _ (m, _, _) _) = m
    getCitoyenMoney (Habitant _ (m, _, _) (_,_,_) _) = m
    getCitoyenMoney _ = 0

calculateMoney :: Ville -> Int
calculateMoney ville = foldr (+) 0 $ map moneyFromZone (Map.elems $ viZones ville)

-- Calculer l'argent d'une zone spécifique
moneyFromZone :: Zone -> Int
moneyFromZone zone = sum $ map moneyFromBuilding (buildingsFromZone zone)

-- Calculer l'argent d'un bâtiment
moneyFromBuilding :: Batiment -> Int
moneyFromBuilding (Epicerie (Rectangle _ w h) _ _ _ _) = w * h
moneyFromBuilding (Cabane _ _ n _ _) = 20 * n
moneyFromBuilding (Atelier (Rectangle _ w h) _ _ _ _) = w * h  -- Si vous souhaitez également accumuler de l'argent pour les ateliers
moneyFromBuilding _ = 0  -- Pour tous les autres types ou formes non gérées

-- Convertit un immigrant en habitant et l'ajoute à une cabane si possible
addImmigrantToCabane :: CitId -> BatId -> Ville -> Maybe Ville
addImmigrantToCabane citId batId ville =
    case Map.lookup citId (viCit ville) of
        Just (Immigrant coord info occupation) -> -- Seulement traiter si c'est un immigrant
            case findCabane batId ville of
                Just (Cabane forme cabCoord capacite residents batId) | length residents < capacite ->
                    let newResidents = residents ++ [citId]
                        updatedCabane = Cabane forme cabCoord capacite newResidents batId
                        newVille = updateVilleWithCabane batId updatedCabane ville
                        newCitoyen = Habitant coord info (batId, Nothing, Nothing) occupation
                        updatedCitoyens = Map.insert citId newCitoyen (viCit ville)
                    in Just newVille { viCit = updatedCitoyens }
                _ -> Nothing -- Soit le bâtiment n'est pas une cabane, soit il est plein
        _ -> Nothing -- Pas un immigrant ou CitId non trouvé


-- Trouve une cabane par BatId dans la ville
findCabane :: BatId -> Ville -> Maybe Batiment
findCabane batId ville = 
    let zones = Map.elems (viZones ville)
        buildings = concatMap buildingsFromZone zones  -- Appliquer buildingsFromZone à chaque zone
    in find (\b -> getBatId b == Just batId && isCabane b) buildings


isCabane :: Batiment -> Bool
isCabane (Cabane _ _ _ _ _) = True
isCabane _ = False

getBatId :: Batiment -> Maybe BatId
getBatId (Cabane _ _ _ _ id) = Just id
getBatId _ = Nothing


updateVilleWithCabane :: BatId -> Batiment -> Ville -> Ville
updateVilleWithCabane batId cabane ville = 
    let updatedZones = Map.map (updateZoneWithCabane batId cabane) (viZones ville)
    in ville { viZones = updatedZones }

-- Met à jour la zone avec la nouvelle cabane
updateZoneWithCabane :: BatId -> Batiment -> Zone -> Zone
updateZoneWithCabane batId cabane zone = case zone of
    ZR forme batiments -> ZR forme (cabane : batiments)  -- Ajoute la cabane à la liste existante des bâtiments
    ZI forme batiments -> ZI forme batiments             -- Garde les autres types de zones inchangées
    ZC forme batiments -> ZC forme batiments
    Admin forme batiment -> Admin forme batiment
    _ -> zone  -- Pour Eau et Route, ou toute autre zone non modifiable pour les bâtiments
