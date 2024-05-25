module SimCity where
import Debug.Trace (trace)
import qualified Data.Map as Map
import qualified Data.Map as Map
import qualified Data.Map.Strict as MapS
import qualified Data.PQueue.Min as PQ
import Data.List (find)
import Data.List (find)
import Data.Maybe (fromMaybe)
import qualified PathFind as PathFind
import Data.Maybe (fromJust, fromMaybe)
import Test.QuickCheck


data Coord = C {cx :: Int, cy :: Int} deriving (Eq, Ord)

instance Show Coord where
    show (C x y) = "Coord (" ++ show x ++ ", " ++ show y ++ ")"

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

contient :: Forme -> [Coord]
contient (HSegment (C x y) longueur)
    | longueur > 0 = (C x y) : contient (HSegment (C (x + 1) y) (longueur - 1))
    | otherwise = []
contient (VSegment (C x y) hauteur)
    | hauteur > 0 = (C x y) : contient (VSegment (C x (y - 1)) (hauteur - 1))
    | otherwise = []
contient (Rectangle (C x y) largeur hauteur)
    | largeur > 0 && hauteur > 0 = [C i j | j <- [y, y-1 .. y - hauteur + 1], i <- [x .. x + largeur - 1]]
    | otherwise = []


aire :: Forme -> Int
aire (HSegment _ longueur) = longueur
aire (VSegment _ hauteur) = hauteur
aire (Rectangle _ largeur hauteur) = largeur * hauteur

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
    marge = 50
    (nord1, sud1, ouest1, est1) = limites forme1
    (nord2, sud2, ouest2, est2) = limites forme2

    -- Vérifie qu'il n'y a pas de chevauchement
    nonChevauchement = not (collisionManuelle forme1 forme2)

    -- Vérifie l'adjacence horizontale avec une marge d'erreur
    horizontalementAdjacente =
      (est1 + marge >= ouest2 && ouest1 - marge <= est2) &&
      ( (nord1 <= sud2 && nord1 >= nord2) || (sud1 >= nord2 && sud1 <= sud2) ||
        (nord2 <= sud1 && nord2 >= nord1) || (sud2 >= nord1 && sud2 <= sud1) )

    -- Vérifie l'adjacence verticale avec une marge d'erreur
    verticalementAdjacente =
      (sud1 + marge >= nord2 && nord1 - marge <= sud2) &&
      ( (ouest1 <= est2 && ouest1 >= ouest2) || (est1 >= ouest2 && est1 <= est2) ||
        (ouest2 <= est1 && ouest2 >= ouest1) || (est2 >= ouest1 && est2 <= est1) )
-- à verifier


newtype ZoneId = ZoneId Int deriving (Show,Eq, Ord)
newtype BatId = BatId Int deriving (Eq, Show)
newtype CitId = CitId Int deriving (Eq, Ord,Show)

data Batiment = Cabane Forme Coord Int [CitId] BatId
    | Atelier Forme Coord Int [CitId] BatId
    | Epicerie Forme Coord Int [CitId] BatId
    | Commissariat Forme Coord BatId
    deriving (Eq)

instance Show Batiment where
    show (Cabane forme coord capacite residents batId) = 
        "Cabane " ++ show forme ++ " " ++ show coord ++ " " ++ show capacite ++ " " ++ show residents ++ " " ++ show batId
    show (Atelier forme coord capacite residents batId) = 
        "Atelier " ++ show forme ++ " " ++ show coord ++ " " ++ show capacite ++ " " ++ show residents ++ " " ++ show batId
    show (Epicerie forme coord capacite residents batId) = 
        "Epicerie " ++ show forme ++ " " ++ show coord ++ " " ++ show capacite ++ " " ++ show residents ++ " " ++ show batId
    show (Commissariat forme coord batId) = 
        "Commissariat " ++ show forme ++ " " ++ show coord ++ " " ++ show batId


data Zone = Eau Forme
    | Route Forme
    | Cable Forme
    | ZR Forme [Batiment]
    | ZI Forme [Batiment] 
    | ZC Forme [Batiment] 
    | ZE Forme
    | Admin Forme Batiment


instance Show Zone where
    show (Eau forme) = "Eau " ++ show forme
    show (Route forme) = "Route " ++ show forme
    show (ZR forme batiments) = "ZR " ++ show forme ++ " " ++ show batiments
    show (ZI forme batiments) = "ZI " ++ show forme ++ " " ++ show batiments
    show (ZC forme batiments) = "ZC " ++ show forme ++ " " ++ show batiments
    show (ZE forme) = "Centrale Elec " ++ show forme 
    show (Cable forme) = "Centrale Elec " ++ show forme 
    show (Admin forme batiment) = "Admin " ++ show forme ++ " " ++ show batiment


instance Eq Zone where -- TODO : vérifir que la liste batiment est la même, créer une fonction pour ça
    (Eau f1) == (Eau f2) = f1 == f2
    (Route f1) == (Route f2) = f1 == f2
    (ZR f1 _) == (ZR f2 _) = f1 == f2
    (ZI f1 _) == (ZI f2 _) = f1 == f2
    (ZC f1 _) == (ZC f2 _) = f1 == f2
    (Admin f1 _) == (Admin f2 _) = f1 == f2
    (ZE f1 ) == (ZE f2) = f1 == f2
    (Cable f1) == (Cable f2) = f1 == f2
    _ == _ = False

data Occupation = Travaille
  | Chomage
  | Dors
  | FaisLesCourses
  | SeDeplaceVers [Coord]
  deriving ( Eq)

instance Show Occupation where
    show Travaille = "Travaille"
    show Chomage = "Chomage"
    show Dors = "Dors"
    show FaisLesCourses = "FaisLesCourses"
    show (SeDeplaceVers path) = "SeDeplaceVers " ++ show path


data Citoyen = Immigrant Coord (Int, Int, Int) Occupation
    | Habitant Coord (Int, Int, Int) (BatId, Maybe BatId, Maybe BatId) Occupation 
    | Emigrant Coord Occupation

instance Show Citoyen where
    show (Immigrant coord stats occupation) = 
        "Immigrant " ++ show coord ++ " " ++ show stats ++ " " ++ show occupation
    show (Habitant coord stats (maison, travail, courses) occupation) = 
        "Habitant " ++ show coord ++ " " ++ show stats ++ " (" ++ show maison ++ ", " ++ show travail ++ ", " ++ show courses ++ ") " ++ show occupation
    show (Emigrant coord occupation) = 
        "Emigrant " ++ show coord ++ " " ++ show occupation



zoneForme::Zone -> Forme
zoneForme (Eau forme) = forme
zoneForme (Route forme) = forme
zoneForme (ZR forme _) = forme
zoneForme (ZI forme _) = forme
zoneForme (ZC forme _) = forme
zoneForme (Admin forme _) = forme
zoneForme (ZE forme ) = forme
zoneForme (Cable forme ) = forme


data Ville = V { viZones :: Map.Map ZoneId Zone, viCit :: Map.Map CitId Citoyen }

instance Show Ville where
    show (V zones citoyens) = "Ville { Zones: " ++ show (Map.toList zones) ++ ", Citoyens: " ++ show (Map.toList citoyens) ++ " }"



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

verifieRouteCentraleCable ::Zone -> Bool
verifieRouteCentraleCable (Route _) = True
verifieRouteCentraleCable (Eau _) = True
verifieRouteCentraleCable (Cable _) = True
verifieRouteCentraleCable (ZE _) = True
verifieRouteCentraleCable _ = False

verifieRoute ::Zone -> Bool
verifieRoute (Route _) = True
verifieRoute _ = False

verifieAdjacenceAuneRoute :: Zone -> Ville -> Bool
verifieAdjacenceAuneRoute zone ville =
    Map.foldr step False (viZones ville)
    where
        step zoneCurrent acc = acc || (verifieRouteCentraleCable zoneCurrent && adjacentes (zoneForme zoneCurrent) (zoneForme zone))

-- Propriété : toutes les zones doivent être adjacentes à une route
prop_verifieAllZonesAdjacentesRoute :: Ville -> Bool
prop_verifieAllZonesAdjacentesRoute ville =
    Map.foldr step True (viZones ville)
    where
        step zone acc = acc && (verifieRouteCentraleCable zone || verifieAdjacenceAuneRoute zone ville)




prop_ville :: Ville -> Bool
prop_ville ville = prop_ville_sansCollision ville && prop_verifieAllZonesAdjacentesRoute ville 

construit :: Ville -> Zone -> Ville
construit ville@(V zones cit) z =
    let newVille = V (Map.insert (ZoneId (Map.size zones)) z zones) cit
    in newVille
    --in if prop_ville newVille
      -- then newVille
      -- else trace "Ajout de la zone échoué: la ville ne respecte pas les propriétés requises." ville




-- TODO : Question 1.6

pre_construit :: Ville -> Zone -> Bool
pre_construit ville zone =
    let newZoneId = ZoneId $ Map.size (viZones ville) + 1  in
    let mapAfter = Map.insert newZoneId zone (viZones ville) in 
    let ville2 = V mapAfter (viCit ville)  
    in prop_ville ville2
    -- in prop_ville_sansCollision ville2
    --in prop_verifieAllZonesAdjacentesRoute ville2
    -- in prop_verifieAllZonesAdjacentesRoute ville2 -- && prop_ville_sansCollision ville2



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


getCapacity :: Batiment -> Int
getCapacity (Cabane _ _ capacity _ _) = capacity
getCapacity (Epicerie _ _ capacity _ _) = capacity
getCapacity (Atelier _ _ capacity _ _) = capacity
getCapacity _ = 0  -- Return 0 for buildings without a defined capacity


getResidents :: Batiment -> [CitId]
getResidents (Cabane _ _ _ residents _) = residents
getResidents (Epicerie _ _ _ workers _) = workers
getResidents (Atelier _ _ _ workers _) = workers
getResidents _ = []  -- Return an empty list for buildings without residents/workers


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
buildingsFromZone _ = []  -- Eau and Route have no buildings, idem for wires and electrical central

updateBuildingsFromZone :: [Batiment] -> Zone -> Zone
updateBuildingsFromZone buildings (ZR tmp bldgs) = ZR tmp buildings
updateBuildingsFromZone buildings (ZI tmp bldgs) = ZI tmp buildings
updateBuildingsFromZone buildings (ZC tmp bldgs) = ZC tmp buildings
updateBuildingsFromZone _ tmp = tmp

batIdFromBuilding :: Batiment -> BatId
batIdFromBuilding (Cabane _ _ _ _ id) = id
batIdFromBuilding (Atelier _ _ _ _ id) = id
batIdFromBuilding (Epicerie _ _ _ _ id) = id
batIdFromBuilding (Commissariat _ _ id) = id

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



{- POST/PRE INVARIANT CHANGER OCCUPATION-}


preconditionChangerOccupation :: Citoyen -> Occupation -> Bool
preconditionChangerOccupation citoyen new_occupation = new_occupation /= undefined

postconditionChangerOccupation :: Citoyen -> Occupation -> Bool
postconditionChangerOccupation citoyen new_occupation =
    let updatedCitoyen = changerOccupation citoyen new_occupation
    in getOccupation updatedCitoyen == new_occupation
  where
    getOccupation (Immigrant _ _ occ) = occ
    getOccupation (Habitant _ _ _ occ) = occ
    getOccupation (Emigrant _ occ) = occ

invariantChangerOccupation :: Citoyen -> Occupation -> Bool
invariantChangerOccupation citoyen new_occupation =
    let updatedCitoyen = changerOccupation citoyen new_occupation
    in case (citoyen, updatedCitoyen) of
        (Immigrant a b _, Immigrant a' b' _) -> a == a' && b == b'
        (Habitant a b c _, Habitant a' b' c' _) -> a == a' && b == b' && c == c'
        (Emigrant a _, Emigrant a' _) -> a == a'
        _ -> False


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
addZone cable@(Cable _) ville = addCable cable ville
addZone zone ville = 
   if pre_construit ville zone then
       let villeApres = construit ville zone -- Suppose que Sim.addZone ajoute simplement la zone sans autres vérifications
       in if post_construit ville zone villeApres then
           villeApres  -- Retourne la ville modifiée si la postcondition est validée
       else trace ("Postcondition failed for zone: " ++ show zone) ville  -- Affiche un message d'erreur si la postcondition échoue
    else trace ("Precondition failed for zone: " ++ show zone) ville  -- Affiche un message d'erreur si la précondition échoue

--sans les pre/posts pck bug
addZone zone ville = construit ville zone





buildingPrice :: Batiment -> Int
buildingPrice (Cabane forme _ _ _ _) = 5 * (aire forme)
buildingPrice (Atelier forme _ _ _ _) = 7 * (aire forme)
buildingPrice (Epicerie forme _ _ _ _) = 10 * (aire forme)
buildingPrice (Commissariat forme _ _) = 15 * (aire forme)

-- Function to add a new building to a zone
addBuildingToZone :: Int -> Batiment -> ZoneId -> Ville -> (Ville, Int)
addBuildingToZone argent newBuilding zoneId ville = 
    case Map.lookup zoneId (viZones ville) of
        Just zone ->
            if buildingInCorrectZone(newBuilding) zone then
                if argent > buildingPrice newBuilding then
                    let updatedZone = updateZoneWithBuilding  newBuilding zone
                        updatedVille = ville { viZones = Map.insert zoneId updatedZone (viZones ville) }
                        updatedArgent = argent - buildingPrice newBuilding
                    in trace (show updatedVille) (updatedVille, updatedArgent)
                else (ville, argent)
            else (ville, argent)
        Nothing -> error "erreur addBuildingToZone"


{- CONDITIONS addBuildingZone-}
preconditionAddBuildingToZone :: Int -> Batiment -> ZoneId -> Ville -> Bool
preconditionAddBuildingToZone argent newBuilding zoneId ville =
    argent > 0 && buildingPrice newBuilding >= 0 && Map.member zoneId (viZones ville)

postconditionAddBuildingToZone :: Int -> Batiment -> ZoneId -> Ville -> (Ville, Int) -> Bool
postconditionAddBuildingToZone argent newBuilding zoneId ville (newVille, newArgent) =
    let oldZone = Map.lookup zoneId (viZones ville)
        newZone = Map.lookup zoneId (viZones newVille)
    in (argent > buildingPrice newBuilding && newArgent == argent - buildingPrice newBuilding) &&
       case (oldZone, newZone) of
           (Just oz, Just nz) -> buildingInCorrectZone newBuilding oz && nz == updateZoneWithBuilding  newBuilding oz
           _ -> False


invariantAddBuildingToZone :: Int -> Batiment -> ZoneId -> Ville -> (Ville, Int) -> Bool
invariantAddBuildingToZone argent newBuilding zoneId ville (newVille, newArgent) =
    argent == newArgent + buildingPrice newBuilding || argent == newArgent





-- Helper function to add a building to the appropriate zone type
updateZoneWithBuilding :: Batiment -> Zone -> Zone
updateZoneWithBuilding  newBuilding (ZR forme bats) = ZR forme (newBuilding : bats)
updateZoneWithBuilding  newBuilding (ZI forme bats) = ZI forme (newBuilding : bats)
updateZoneWithBuilding  newBuilding  (ZC forme bats) = ZC forme (newBuilding : bats)
updateZoneWithBuilding  _ other = other



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
    viZones = Map.fromList [
       --(ZoneId 1, Route (Rectangle (C 0 0) 50 400))
      --  ,(ZoneId 2, Route (Rectangle (C 50 200) 100 50))
    ],
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

moneyFromBuilding :: Batiment -> Int
moneyFromBuilding (Epicerie (Rectangle  w h)   workers ) = 30 * length workers
moneyFromBuilding (Cabane   n  ) = 20 * n
moneyFromBuilding (Atelier (Rectangle  w h)   workers ) = 30 * length workers
moneyFromBuilding _ = 0  -- Pour tous les autres types ou formes non gérées

{-    
addImmigrantToCabane :: CitId -> BatId -> Ville -> Maybe Ville
addImmigrantToCabane citId batId ville =
    case Map.lookup citId (viCit ville) of
        Just (Immigrant coord info occupation) ->
            case findCabane batId ville of
                Just (Cabane forme cabCoord capacite residents batId) | length residents < capacite ->
                    case (findNearestRoute coord ville, findNearestRoute cabCoord ville) of
                        (Just startRoute, Just endRoute) ->
                            let startCoord = moveToRoute coord startRoute
                                endCoord = moveToRoute cabCoord endRoute
                            in case aStar startCoord endCoord ville of
                                Just path ->
                                    let newResidents = residents ++ [citId]
                                        updatedCabane = Cabane forme cabCoord capacite newResidents batId
                                        newVille = updateVilleWithCabane batId updatedCabane ville
                                        fullPath = [startCoord] ++ path ++ [endCoord, cabCoord] -- ajout des déplacements directs
                                        _ = trace ("startPoint: " ++ show startCoord) ()  -- Afficher le chemin complet

                                        _ = trace ("fullPath: " ++ show fullPath) ()  -- Afficher le chemin complet
                                        newCitoyen = Habitant coord info (batId, Nothing, Nothing) (SeDeplaceVers fullPath)
                                        updatedCitoyens = Map.insert citId newCitoyen (viCit ville)
                                    in Just newVille { viCit = updatedCitoyens }
                                Nothing -> trace "No path found" Nothing
                        _ -> trace "Route not found" Nothing -- Une des routes n'a pas été trouvée
                _ -> trace "Cabane is full or not found" Nothing -- La cabane est pleine ou non trouvée
        _ -> trace "Immigrant not found" Nothing -- Immigrant non trouvé
-}


addImmigrantToCabane :: CitId -> BatId -> Ville -> Maybe Ville
addImmigrantToCabane citId batId ville =
    case Map.lookup citId (viCit ville) of
        Just (Immigrant coord info occupation) ->
            case findCabane batId ville of
                Just (Cabane forme cabCoord capacite residents batId) | length residents < capacite ->
                    let newResidents = residents ++ [citId]
                        updatedCabane = Cabane forme cabCoord capacite newResidents batId
                        newCitoyen = Habitant cabCoord (400,400,400) (batId, Nothing, Nothing) Chomage
                        updatedCitoyens = Map.insert citId newCitoyen (viCit ville)
                        newVille = updateVilleWithBuilding updatedCabane ville
                    in Just newVille { viCit = updatedCitoyens }
                _ -> trace "Cabane is full or not found" Nothing
        _ -> trace "Immigrant not found" Nothing
{-Conditions addImmigrantToCabane-}

preconditionAddImmigrantToCabane :: CitId -> BatId -> Ville -> Bool
preconditionAddImmigrantToCabane citId batId ville =
    Map.member citId (viCit ville) &&
    case Map.lookup citId (viCit ville) of
        Just (Immigrant _ _ _) -> True
        _ -> False


postconditionAddImmigrantToCabane :: CitId -> BatId -> Ville -> Bool
postconditionAddImmigrantToCabane citId batId ville =
    case Map.lookup citId (viCit ville) of
        Just (Habitant _ (400,400,400) (newBatId, Nothing, Nothing) Chomage) ->
            newBatId == batId
        _ -> False

invariantAddImmigrantToCabane :: CitId -> BatId -> Ville -> Bool
invariantAddImmigrantToCabane citId batId ville =
    case Map.lookup citId (viCit ville) of
        Just (Habitant _ (400,400,400) (newBatId, Nothing, Nothing) Chomage) ->
            let cabane = findCabane batId ville
            in case cabane of
                Just (Cabane _ _ capacite residents _) -> length residents <= capacite
                _ -> False
        _ -> False



-- Trouve une cabane par BatId dans la ville
findCabane :: BatId -> Ville -> Maybe Batiment
findCabane batId ville = 
    let zones = Map.elems (viZones ville)
        buildings = concatMap buildingsFromZone zones  -- Appliquer buildingsFromZone à chaque zone
    in find (\b -> getBatId b == batId && isCabane b) buildings


isCabane :: Batiment -> Bool
isCabane (Cabane _ _ _ _ _) = True
isCabane _ = False

getBatId :: Batiment ->  BatId
getBatId (Cabane _ _ _ _ id) =  id
getBatId (Atelier  _ _ _ _ id )= id 
getBatId (Epicerie  _ _ _ _  id )= id 
getBatId (Commissariat  _ _ id )= id 


updateVilleWithBuilding :: Batiment -> Ville -> Ville
updateVilleWithBuilding  cabane ville = 
    let updatedZones = Map.map (updateZoneWithBuilding cabane) (viZones ville)
    in ville { viZones = updatedZones }


findFirstImmigrantId :: Ville -> Maybe CitId
findFirstImmigrantId ville = fmap fst $ find (isImmigrant . snd) (Map.toList (viCit ville))

isImmigrant :: Citoyen -> Bool
isImmigrant (Immigrant _ _ _) = True
isImmigrant _ = False

addImmigrants :: Int -> Ville -> CitId -> (Ville, CitId)
addImmigrants count ville (CitId startId) =
    let existingImmigrants = Map.size $ Map.filter isImmigrant (viCit ville)
    in if existingImmigrants == 0
       then let newCitizens = [Immigrant (C 0 0) (200, 0, 0) Chomage | _ <- [1..count]]
                newIds = [CitId (startId + i) | i <- [1..count]]
                updatedMap = foldl (\acc (cit, CitId id) -> Map.insert (CitId id) cit acc) (viCit ville) (zip newCitizens newIds)
                newCitId = CitId (startId + count)
            in (ville { viCit = updatedMap }, newCitId)
       else (ville, CitId startId)
{-Invariant addImmigrant-}
preconditionAddImmigrants :: Int -> Ville -> CitId -> Bool
preconditionAddImmigrants _ ville _ = 
    Map.size (Map.filter isImmigrant (viCit ville)) == 0


postconditionAddImmigrants :: Int -> Ville -> CitId -> Ville -> CitId -> Bool
postconditionAddImmigrants count oldVille oldCitId newVille newCitId =
    let newImmigrants = Map.filter isImmigrant (viCit newVille)
        expectedNewIds = [CitId (getCitId oldCitId + i) | i <- [1..count]]
        newCitizenIds = Map.keys newImmigrants
    in length newCitizenIds == count &&
       all (`elem` newCitizenIds) expectedNewIds &&
       newCitId == CitId (getCitId oldCitId + count)
  where
    getCitId (CitId cid) = cid





-- Extrait l'int d'un CitId
getCitId :: CitId -> Int
getCitId (CitId id) = id


incrementBatId::BatId -> BatId
incrementBatId (BatId id) = BatId (id+1)

citizenCoord :: Citoyen -> Coord
citizenCoord (Immigrant res _ _) = res
citizenCoord (Habitant res _ _ _) = res
citizenCoord (Emigrant res _) = res


getAllCitizens :: Ville -> [Citoyen]
getAllCitizens ville = Map.elems (viCit ville)

getCitizenForme :: Citoyen -> Forme
getCitizenForme citoyen = case citoyen of
    Immigrant coord _ _ -> Rectangle coord 30 80
    Habitant coord _ _ _ -> Rectangle coord 30 80
    Emigrant coord _ -> Rectangle coord 30 80




isRoute :: Zone -> Bool
isRoute (Route _) = True
isRoute _ = False


moveToRoute :: Coord -> Forme -> Coord
moveToRoute (C x y) (HSegment (C rx ry) length) = C rx y -- Se déplace horizontalement vers la route
moveToRoute (C x y) (VSegment (C rx ry) height) = C x ry -- Se déplace verticalement vers la route
moveToRoute (C x y) (Rectangle (C rx ry) _ _) = C rx ry


moveFromRouteToDestination :: Ville -> Citoyen -> Coord -> Maybe Citoyen
moveFromRouteToDestination ville citoyen destination =
    case findNearestRoute destination ville of
        Just nearestRoute -> Just $ changeOccupation citoyen (SeDeplaceVers [moveToRoute (citizenCoord citoyen) nearestRoute, destination])
        Nothing -> Nothing




         


updateCitizenPosition :: Ville -> Citoyen -> Citoyen
updateCitizenPosition ville citoyen = case citoyen of
    Habitant coord bio batid occ -> updateOccupation ville coord occ
    Immigrant coord bio occ -> updateOccupation ville coord occ
    Emigrant coord occ -> updateOccupation ville coord occ
  where
    updateOccupation ville coord (SeDeplaceVers [] ) = changeOccupation citoyen Travaille  -- Change occupation to 'Travaille' if the path is empty
    updateOccupation ville coord (SeDeplaceVers (nextCoord:path) ) = changeCoord citoyen nextCoord path -- Moves to the next coordinate and updates the path
    updateOccupation ville coord occ = citoyen  -- No update if not moving


changeCoord :: Citoyen -> Coord -> [Coord] -> Citoyen
changeCoord (Habitant _ bio batid occ) newCoord path = Habitant newCoord bio batid occ
changeCoord (Immigrant _ bio occ) newCoord path = Immigrant newCoord bio occ
changeCoord (Emigrant _ occ) newCoord path = Emigrant newCoord occ

changeOccupation :: Citoyen -> Occupation -> Citoyen
changeOccupation (Habitant coord bio batid _) newOcc = Habitant coord bio batid newOcc
changeOccupation (Immigrant coord bio _) newOcc = Immigrant coord bio newOcc
changeOccupation (Emigrant coord _) newOcc = Emigrant coord newOcc

moveTowards :: Coord -> Coord -> Coord
moveTowards (C x1 y1) (C x2 y2) = 
    let dx = signum (x2 - x1)
        dy = signum (y2 - y1)
    in C (x1 + dx) (y1 + dy)



-- AStar

-- Obtenir les coordonnées d'une zone à partir d'une coordonnée donnée
getZoneCoords :: Coord -> Ville -> [Coord]
getZoneCoords coord ville = 
    let zones = getZones ville
        maybeZone = find (\zone -> appartient coord (zoneForme zone)) zones
    in case maybeZone of
        Just zone -> contient (zoneForme zone)
        Nothing -> []


getNeighbors :: Coord -> Ville -> [Coord]
getNeighbors coord ville =
    let routes = filter isRoute $ getZones ville
        routeForms = map zoneForme routes
        adjacentCoords = concatMap (generateCoordsOnRoute coord) routeForms
        zoneCoords = getZoneCoords coord ville  -- Ajouter les coordonnées de la zone de départ
    in filter (\c -> any (appartient c) routeForms || c `elem` zoneCoords) adjacentCoords  -- Inclure les coordonnées de la zone de départ

generateCoordsOnRoute :: Coord -> Forme -> [Coord]
generateCoordsOnRoute (C x y) routeForm =
    case routeForm of
        HSegment (C rx ry) len ->
            [C (rx + dx) ry | dx <- [-1, 1], rx + dx <= rx + len, rx + dx >= rx]
        VSegment (C rx ry) len ->
            [C rx (ry + dy) | dy <- [-1, 1], ry + dy <= ry + len, ry + dy >= ry]
        _ -> []

isOnRoute :: Coord -> [Forme] -> Bool
isOnRoute coord routes = any (appartient coord) routes


extractRouteNeighbors :: Coord -> Forme -> [Coord]
extractRouteNeighbors (C x y) routeForm =
    case routeForm of
        HSegment (C rx ry) length -> if y == ry then [C (rx + dx) y | dx <- [-1..1], dx /= 0] else []
        VSegment (C rx ry) height -> if x == rx then [C x (ry + dy) | dy <- [-1..1], dy /= 0] else []
        _ -> [] -- Ignoring non-segment route forms or can extend to handle rectangles if needed



-- Function to reconstruct the path from the current node to the start using the cameFrom map
reconstructPath :: Map.Map Coord Coord -> Coord -> [Coord] -> [Coord]
reconstructPath cameFrom current path = 
    case Map.lookup current cameFrom of
        Just prev -> reconstructPath cameFrom prev (current : path)
        Nothing -> current : path  -- Base case: start of the path


aStar :: Coord -> Coord -> Ville -> Maybe [Coord]
aStar start goal ville = 
    let 
        nearestStartRoute = findNearestRoute start ville
        nearestEndRoute = findNearestRoute goal ville
    in case (nearestStartRoute, nearestEndRoute) of
        (Just startRoute, Just endRoute) -> 
            let startCoord = moveToRoute start startRoute
                endCoord = moveToRoute goal endRoute
            in aStarHelper startCoord endCoord ville
        _ -> Nothing

aStarHelper :: Coord -> Coord -> Ville -> Maybe [Coord]
aStarHelper start goal ville = go (PQ.singleton (heuristic start goal, start)) Map.empty (Map.singleton start 0) (Map.singleton start (heuristic start goal))
  where
    go openSet cameFrom gScore fScore
        | PQ.null openSet = Nothing
        | otherwise = 
            let ((_, current), openRest) = PQ.deleteFindMin openSet
            in if current == goal 
               then Just (reconstructPath cameFrom current [])
               else 
                    let neighbors = getNeighbors current ville
                        (newOpenSet, newGScore, newFScore) = foldr (processNeighbor current) (openRest, gScore, fScore) neighbors
                    in go newOpenSet (Map.insert current (fromMaybe current (Map.lookup current cameFrom)) cameFrom) newGScore newFScore

    processNeighbor current neighbor (openSet, gScore, fScore) =
        let tentativeGScore = fromMaybe maxBound (Map.lookup current gScore) + 1
            gScoreNeighbor = fromMaybe maxBound (Map.lookup neighbor gScore)
        in if tentativeGScore < gScoreNeighbor
           then 
                let newGScore = Map.insert neighbor tentativeGScore gScore
                    newFScore = Map.insert neighbor (tentativeGScore + heuristic neighbor goal) fScore
                    newOpenSet = PQ.insert (newFScore Map.! neighbor, neighbor) openSet
                in (newOpenSet, newGScore, newFScore)
           else (openSet, gScore, fScore)

    heuristic (C x1 y1) (C x2 y2) = abs (x2 - x1) + abs (y2 - y1)


moveToNearestRoute :: Ville -> Citoyen -> Maybe Citoyen
moveToNearestRoute ville citoyen =
    case findNearestRoute (citizenCoord citoyen) ville of
        Just nearestRoute -> Just $ changeOccupation citoyen (SeDeplaceVers [moveToRoute (citizenCoord citoyen) nearestRoute])
        Nothing -> Nothing


planCitizenMovement :: Ville -> Citoyen -> Coord -> IO Citoyen
planCitizenMovement ville citoyen destination = do
    let start = citizenCoord citoyen
    let updatedCitizen = moveToNearestRoute ville citoyen
    case updatedCitizen of
        Just updatedCitizen' ->
            case findNearestRoute destination ville of
                Just nearestRouteToDestination ->
                    case aStar (citizenCoord updatedCitizen') (moveToRoute destination nearestRouteToDestination) ville of
                        Just path -> return $ changeOccupation updatedCitizen' (SeDeplaceVers path)
                        Nothing -> return citoyen
                Nothing -> return citoyen
        Nothing -> return citoyen

findNearestRoute :: Coord -> Ville -> Maybe Forme
findNearestRoute coord ville = 
    let routes = map zoneForme $ filter isRoute (getZones ville)
    in if null routes
       then Nothing
       else Just $ foldr1 (\r1 r2 -> if distance coord r1 < distance coord r2 then r1 else r2) routes

-- Fonction pour calculer la distance entre une coordonnée et une forme
distance :: Coord -> Forme -> Int
distance (C x y) (HSegment (C rx ry) length) =
    if y == ry
    then min (abs (x - rx)) (abs (x - (rx + length)))
    else abs (y - ry) + min (abs (x - rx)) (abs (x - (rx + length)))
distance (C x y) (VSegment (C rx ry) height) =
    if x == rx
    then min (abs (y - ry)) (abs (y - (ry + height)))
    else abs (x - rx) + min (abs (y - ry)) (abs (y - (ry + height)))
distance (C x y) (Rectangle (C rx ry) width height) =
    let dx = if x < rx then rx - x else if x > rx + width then x - (rx + width) else 0
        dy = if y < ry then ry - y else if y > ry + height then y - (ry + height) else 0
    in dx + dy

findNearestPointOnEndRoute :: Coord -> Forme -> Coord
findNearestPointOnEndRoute (C x y) routeForm = 
    case routeForm of
        HSegment (C rx ry) length -> C (max rx (min (rx + length) x)) ry
        VSegment (C rx ry) height -> C rx (max ry (min (ry + height) y))
        Rectangle (C rx ry) width height -> 
            let nearestX = max rx (min (rx + width) x)
                nearestY = max ry (min (ry + height) y)
            in C nearestX nearestY



-- fin A*
--Mise à jour des citoyens en simulant un roulement : travail -> dort/course -> travail, les chomeurs font rien 

updateCitizens :: Ville -> Ville
updateCitizens ville@(V vizones vicit) = 
    V vizones (Map.map (updateCitizenBasedOnOccupation ville) vicit)
  where
    updateCitizenBasedOnOccupation ville citoyen =
        case getOccupation citoyen of
            Travaille ->updateTravail ville citoyen
            Dors -> updateDormir ville citoyen
            FaisLesCourses -> updateFaisLesCourses ville citoyen
            _ ->  citoyen



updateTravail :: Ville -> Citoyen -> Citoyen
updateTravail ville citoyen@(Habitant coord (argent, fatigue, faim) (maison, Just travail, courses) occupation)
    | fatigue <= 0 =
        case findBuilding maison ville of
            Just maisonCoord -> changeCoord (changeOccupation citoyen Dors) maisonCoord []
            Nothing -> trace "Home not found" citoyen
    | faim <= 0 =
        case courses of
            Just epicerie -> 
                case findBuilding epicerie ville of
                    Just epicerieCoord -> changeCoord (changeOccupation citoyen FaisLesCourses) epicerieCoord []
                    Nothing -> trace "Grocery store not found" citoyen
            Nothing -> trace "No grocery store assigned" citoyen
    | otherwise = Habitant coord (argent + 2, fatigue - 1, faim-2) (maison, Just travail, courses) occupation
updateTravail _ citoyen = citoyen

{-Conditions updateTravail-}
-- Précondition
preconditionUpdateTravail :: Ville -> Citoyen -> Bool
preconditionUpdateTravail _ (Habitant _ _ (_, Just _, _) _) = True
preconditionUpdateTravail _ _ = False

-- Postcondition
postconditionUpdateTravail :: Ville -> Citoyen -> Bool
postconditionUpdateTravail ville citoyen@(Habitant coord (argent, fatigue, faim) (maison, travail, courses) _) =
    let updatedCitoyen = updateTravail ville citoyen
        (Habitant newCoord (newArgent, newFatigue, newFaim) newLogement newOccupation) = updatedCitoyen
    in case newOccupation of
        Dors -> newCoord == fromMaybe coord (findBuilding maison ville) && newFatigue > fatigue
        FaisLesCourses -> newCoord == fromMaybe coord (findBuilding (fromJust courses) ville) && newFaim > faim
        Travaille -> newCoord == coord && newArgent == argent + 2 && newFatigue == fatigue - 1 && newFaim == faim - 2
        _ -> False
postconditionUpdateTravail _ _ = False

-- Invariant
invariantUpdateTravail :: Citoyen -> Bool
invariantUpdateTravail citoyen@(Habitant coord bio logement occupation) = 
    let updatedCitoyen = updateTravail undefined citoyen
        (Habitant newCoord newBio newLogement newOccupation) = updatedCitoyen
    in coord == newCoord && logement == newLogement && bio == newBio
invariantUpdateTravail _ = False

{-fin-}


findBuilding :: BatId -> Ville -> Maybe Coord
findBuilding batId ville = 
    let buildings = getAllBuildings ville
        foundBuilding = find (\b -> getBatId b == batId) buildings
    in getEntry <$> foundBuilding


updateDormir :: Ville -> Citoyen -> Citoyen
updateDormir ville citoyen@(Habitant coord (argent, fatigue, faim) (maison, travail, courses) occupation)
    | fatigue < 200 = trace ("Fatigue: " ++ show fatigue) $ changeOccupation (Habitant coord (argent, fatigue + 1, faim) (maison, travail, courses) occupation) Dors
    | otherwise =
        case travail of
            Just travailId ->
                case findBuilding travailId ville of
                    Just travailCoord -> trace ("Fatigue: " ++ show fatigue) $ changeCoord (changeOccupation citoyen Travaille) travailCoord []
                    Nothing -> trace ("Fatigue: " ++ show fatigue ++ ", Work not found") citoyen
            Nothing -> trace ("Fatigue: " ++ show fatigue ++ ", No work assigned") citoyen
updateDormir _ citoyen = citoyen


{-COnditions pour updateDormir
preconditionUpdateDormir :: Ville -> Citoyen -> Bool
preconditionUpdateDormir _ (Habitant _ _ (Just _, _, _) _) = True
preconditionUpdateDormir _ _ = False


postconditionUpdateDormir :: Ville -> Citoyen -> Citoyen -> Bool
postconditionUpdateDormir ville citoyen@(Habitant coord (argent, fatigue, faim) (maison, Just travail, courses) occupation) updatedCitoyen
    | fatigue < 200 =
        let (Habitant newCoord (newArgent, newFatigue, newFaim) newHabitation newOccupation) = updatedCitoyen
        in newFatigue == fatigue + 1 && newOccupation == Dors && newCoord == coord && newArgent == argent && newFaim == faim && newHabitation == (maison, Just travail, courses)
    | otherwise =
        case findBuilding (fromJust travail) ville of
            Just travailCoord -> 
                let (Habitant newCoord (newArgent, newFatigue, newFaim) newHabitation newOccupation) = updatedCitoyen
                in newCoord == travailCoord && newOccupation == Travaille
            Nothing -> False
postconditionUpdateDormir _ _ _ = False

-}

  


updateFaisLesCourses :: Ville -> Citoyen -> Citoyen
updateFaisLesCourses ville citoyen@(Habitant coord (argent, fatigue, faim) (maison, Just travail, courses) occupation)
    | argent > 2 && faim < 200 = Habitant coord (argent - 2, fatigue, faim + 5) (maison, Just travail, courses) FaisLesCourses
    | otherwise =
        case findBuilding travail ville of
            Just travailCoord -> changeCoord (changeOccupation citoyen Travaille) travailCoord []
            Nothing -> trace "Work not found" citoyen
updateFaisLesCourses _ citoyen = citoyen


--Donner un travail aux habitants



assignWorkToHabitant :: CitId -> BatId -> Ville -> Maybe Ville
assignWorkToHabitant citId batId ville = 
    case Map.lookup citId (viCit ville) of
        Just (Habitant coord info (maison, _, courses) occupation) ->
            case findBuildingById batId (getAllBuildings ville) of
                Just building ->
                    case addCitizenToBuilding citId building of
                        Just updatedBuilding ->
                            let newCitoyen = Habitant coord info (maison, Just batId, courses) Travaille
                                updatedCitoyens = Map.insert citId newCitoyen (viCit ville)
                                updatedVille = updateVilleWithBuilding updatedBuilding ville
                            in Just updatedVille
                        Nothing -> trace ("Building capacity exceeded for BatId: " ++ show batId) Nothing
                Nothing -> trace ("Building not found for BatId: " ++ show batId) Nothing
        _ -> trace ("Habitant not found for CitId: " ++ show citId) Nothing



updateResidents :: ([CitId] -> [CitId]) -> Batiment -> Batiment
updateResidents f (Cabane forme coord capacite residents batId) = Cabane forme coord capacite (f residents) batId
updateResidents f (Atelier forme coord capacite residents batId) = Atelier forme coord capacite (f residents) batId
updateResidents f (Epicerie forme coord capacite residents batId) = Epicerie forme coord capacite (f residents) batId
updateResidents f other = other


replaceBuilding :: Batiment -> [Batiment] -> [Batiment]
replaceBuilding updatedBuilding = map (\b -> if getBatId b == getBatId updatedBuilding then updatedBuilding else b)


addCitizenToBuilding :: CitId -> Batiment -> Maybe Batiment
addCitizenToBuilding citId building = 
    if length (getResidents building) < getCapacity building
    then Just (updateResidents (citId:) building)
    else Nothing

findBuildingById :: BatId -> [Batiment] -> Maybe Batiment
findBuildingById batId = find (\b -> getBatId b ==  batId)  
-- Trouver un habitant sans travail
findFirstHabitantId :: Ville -> Maybe CitId
findFirstHabitantId ville = 
    fmap fst $ find (isUnemployedHabitant . snd) (Map.toList (viCit ville))

isUnemployedHabitant :: Citoyen -> Bool
isUnemployedHabitant (Habitant _ _ (_, Nothing, _) _) = True
isUnemployedHabitant _ = False


--assigner un endroit où se nourrir
-- Fonction pour assigner une épicerie à un habitant
assignGroceryToHabitant :: CitId -> BatId -> Ville -> Maybe Ville
assignGroceryToHabitant citId batId ville = 
    case Map.lookup citId (viCit ville) of
        Just (Habitant coord info (maison, travail, _) occupation) ->
            case findBuilding batId ville of
                Just buildingCoord ->
                    let newCitoyen = Habitant coord info (maison, travail, Just batId) occupation
                        updatedCitoyens = Map.insert citId newCitoyen (viCit ville)
                    in Just ville { viCit = updatedCitoyens }
                Nothing -> trace ("Building not found for BatId: " ++ show batId) Nothing
        _ -> trace ("Habitant not found for CitId: " ++ show citId) Nothing

-- Trouver un habitant sans épicerie assignée
findFirstHabitantWithoutGroceryId :: Ville -> Maybe CitId
findFirstHabitantWithoutGroceryId ville = 
    fmap fst $ find (isHabitantWithoutGrocery . snd) (Map.toList (viCit ville))

isHabitantWithoutGrocery :: Citoyen -> Bool
isHabitantWithoutGrocery (Habitant _ _ (_, _, Nothing) _) = True
isHabitantWithoutGrocery _ = False

--compter les habitants pour le panneau

countCitizens :: Ville -> (Int, Int, Int)
countCitizens ville =
    let citizens = Map.elems (viCit ville)
        immigrants = length (filter isImmigrant citizens)
        emigrants = length (filter isEmigrant citizens)
        habitants = length (filter isHabitant citizens)
    in (immigrants, emigrants, habitants)


isEmigrant :: Citoyen -> Bool
isEmigrant (Emigrant _ _) = True
isEmigrant _ = False

isHabitant :: Citoyen -> Bool
isHabitant (Habitant _ _ _ _) = True
isHabitant _ = False


augmenterCapaciteBatiments :: [Batiment] -> [Batiment]
augmenterCapaciteBatiments = map augmenterCapacite
  where
    augmenterCapacite (Cabane forme coord capacite residents batId) = Cabane forme coord (capacite + 10) residents batId
    augmenterCapacite (Atelier forme coord capacite residents batId) = Atelier forme coord (capacite + 4) residents batId
    augmenterCapacite (Epicerie forme coord capacite residents batId) = Epicerie forme coord (capacite + 4) residents batId
    augmenterCapacite autre = autre

-- Fonction pour vérifier si une zone est adjacente à une autre zone
validAdjacentZone :: Forme -> Zone -> Bool
validAdjacentZone newCableForm zone = case zone of
    ZE forme -> adjacentes forme newCableForm
    Cable forme -> adjacentes forme newCableForm
    _ -> False

-- Mettre à jour la ville avec l'électricité en augmentant la capacité des bâtiments
updateWithElectricity :: Ville -> Ville
updateWithElectricity ville =
    let zones = Map.toList (viZones ville)
        cables = filter (isCable . snd) zones
        updatedZones = foldl updateZoneElectricity (viZones ville) cables
    in ville { viZones = updatedZones }
  where
    isCable (Cable _) = True
    isCable _ = False

    updateZoneElectricity :: Map.Map ZoneId Zone -> (ZoneId, Zone) -> Map.Map ZoneId Zone
    updateZoneElectricity zones (zoneId, Cable forme) =
        let adjacents = filter (adjacentes forme . zoneForme . snd) (Map.toList zones)
        in foldl augmenterCapaciteSiAdjacente zones adjacents

    augmenterCapaciteSiAdjacente :: Map.Map ZoneId Zone -> (ZoneId, Zone) -> Map.Map ZoneId Zone
    augmenterCapaciteSiAdjacente zones (adjZoneId, ZR forme batiments) =
        Map.adjust (\(ZR forme _) -> ZR forme (augmenterCapaciteBatiments batiments)) adjZoneId zones
    augmenterCapaciteSiAdjacente zones (adjZoneId, ZI forme batiments) =
        Map.adjust (\(ZI forme _) -> ZI forme (augmenterCapaciteBatiments batiments)) adjZoneId zones
    augmenterCapaciteSiAdjacente zones (adjZoneId, ZC forme batiments) =
        Map.adjust (\(ZC forme _) -> ZC forme (augmenterCapaciteBatiments batiments)) adjZoneId zones
    augmenterCapaciteSiAdjacente zones _ = zones

-- Fonction pour ajouter un câble à la ville
addCable :: Zone -> Ville -> Ville
addCable zoneCable ville =
    let zones = Map.elems (viZones ville)
        validAdjacentZone zone = case zone of
            ZE forme -> adjacentes forme (zoneForme zoneCable)
            Cable forme -> adjacentes forme (zoneForme zoneCable)
            _ -> False
        isValidCable = any validAdjacentZone zones
    in if isValidCable
       then
           let newZone = Cable (zoneForme zoneCable)
               updatedVille = construit ville newZone
           in (updateWithElectricity updatedVille)
       else ville

updateZoneWithBuildings :: Zone -> [Batiment] -> Zone
updateZoneWithBuildings (ZR forme _) buildings = ZR forme buildings
updateZoneWithBuildings (ZI forme _) buildings = ZI forme buildings
updateZoneWithBuildings (ZC forme _) buildings = ZC forme buildings

-- Fonction pour calculer le coût de maintient de la ville 
cityCost :: Ville -> Int
cityCost ville = 
    Map.foldr step 0 (viZones ville) 
    where
        step (ZE _) acc = acc +2000 -- la ZE coûte 2000 à entretenir
        step (Cable _) acc= 200 + acc -- les cables aussi coûte de l'argent
        step (Admin _ _) acc= acc + 1000 -- la popo coûte aussi faut bien qu'ils mangent
        step (Eau _)acc = acc + 300 -- l'eau aussi, on entretient pour garder un bon score de pollution
        step _ acc= acc -- le reste est considéré comme un investissement 


--Fonction pour calculer le score de pollution, en effet les cables et la zone electrique polluent, cependant l'eau 
-- permet de baisser ce score
pollutionScore:: Ville -> Int
pollutionScore ville = 
     Map.foldr step 0 (viZones ville) 
    where
        step (ZE _) acc = acc + 20 
        step (Cable _) acc= 5 + acc
        step (Eau _) acc = acc - 20 
        step _ acc= acc


getCommissariats :: Ville -> [Batiment]
getCommissariats ville = filter isCommissariat (getAllBuildings ville)
  where
    isCommissariat (Commissariat _ _ _) = True
    isCommissariat _ = False


safetyScore :: Ville -> Int
safetyScore ville =
    let unemployedCount = countUnemployedCitizens ville
        buildingsNearCommissariatsCount = countBuildingsNearCommissariats ville
    in buildingsNearCommissariatsCount * 10 - unemployedCount * 20


countUnemployedCitizens :: Ville -> Int
countUnemployedCitizens ville =
    length $ filter isUnemployed (Map.elems (viCit ville))
  where
    isUnemployed (Habitant _ _ _ Chomage) = True
    isUnemployed _ = False


countBuildingsNearCommissariats :: Ville -> Int
countBuildingsNearCommissariats ville =
    length $ filter isNearCommissariat (getAllBuildings ville)
  where
    commissariats = getCommissariats ville
    isNearCommissariat building = any (\(Commissariat coord _ _) -> distance (getEntry building) coord <= 200) commissariats

    getCommissariats :: Ville -> [Batiment]
    getCommissariats ville = filter isCommissariat (getAllBuildings ville)
    
    isCommissariat (Commissariat _ _ _) = True
    isCommissariat _ = False
