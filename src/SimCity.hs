module SimCity where

import qualified Data.Map as Map

data Coord = C {cx :: Int, cy :: Int} deriving (Show , Eq)

data Forme = HSegment Coord Int
    | VSegment Coord Int
    | Rectangle Coord Int Int

-- (nord, sud, ouest, est)
limites::Forme -> (Int, Int, Int, Int)
limites (HSegment (C x y) longueur) = (y, y, x, x + longueur)
limites (VSegment (C x y) hauteur) = (y, y - hauteur, x, x)
limites (Rectangle (C x y) largeur hauteur) = (y, y - hauteur, x, x + largeur)

appartient::Coord -> Forme -> Bool
appartient (C x1 y1) (HSegment (C x2 y2) longueur) = y1 == y2 && x1 >= x2 && x1 <= (x2 + longueur)
appartient (C x1 y1) (VSegment (C x2 y2) hauteur) = x1 == x2 && y1 <= y2 && y1 >= (y2 - hauteur)
appartient (C x1 y1) (Rectangle (C x2 y2) largeur hauteur) = x1 >= x2 && x1 <= (x2 + largeur) && y1 <= y2 && y1 >= (y2 - hauteur)

adjacent::Coord -> Forme -> Bool
adjacent (C x1 y1) (HSegment (C x2 y2) longueur) 
    | y1 == y2  = (x1 == (x2 - 1)) || (x1 == (x2 + longueur + 1))
    | otherwise = ((x1 >= x2 && x1 <= (x2 + longueur)) && ((y1 == (y2 - 1)) || (y1 == (y2 + 1))))
adjacent (C x1 y1) (VSegment (C x2 y2) hauteur)
    | x1 == x2 = (y1 == (y2 + 1)) || (y1 == (y2 - hauteur - 1))
    | otherwise = (y1 <= y2 && y1 >= (y2 - hauteur)) && ((x1 == (x2 - 1)) || (1 == (x2 + 1)))
adjacent (C x1 y1) (Rectangle (C x2 y2) largeur hauteur)
    | x1 == (x2 - 1) || x1 == (x2 + largeur + 1) = (y1 <= (y2 + 1)) && (y1 >= (y2 - hauteur - 1))
    | y1 == (y2 + 1) || y1 == (y2 - hauteur - 1) = (x1 >= (x2 - 1)) && (x1 <= (x2 + largeur + 1))

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

-- TODO : Selon le sujet, on accepte les faux positifs, qu'est-ce que ça veut dire sur l'implémentation ?
-- double foldr avec contient?
collision_approx::Forme -> Forme -> Bool
collision_approx _ _ = undefined

-- double foldr avec adjacent (quasiment pareil que collision_approx)
adjacentes::Forme -> Forme -> Bool
adjacentes _ _ = undefined


newtype ZoneId = ZoneId Int deriving (Eq, Ord)
newtype BatId = BatId Int 
newtype CitId = CitId String deriving (Eq, Ord)

data Batiment = Cabane Forme Coord Int [CitId] 
    | Atelier Forme Coord Int [CitId]
    | Epicerie Forme Coord Int [CitId] 
    | Commissariat Forme Coord

data Zone = Eau Forme
    | Route Forme
    | ZR Forme [Batiment] 
    | ZI Forme [Batiment] 
    | ZC Forme [Batiment] 
    | Admin Forme Batiment

data Occupation = Travaille
  | Dors
  | FaisLesCourses
  | SeDeplaceVers Coord
  deriving (Show, Eq)

data Citoyen = Immigrant Coord (Int, Int, Int) Occupation
    | Habitant Coord (Int, Int, Int) (BatId, Maybe BatId, Maybe BatId) Occupation 
    | Emigrant Coord Occupation

zoneForme::Zone -> Forme
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
prop_ville_sansCollision::Ville -> Bool
prop_ville_sansCollision = undefined

-- TODO : Question 1.4 : Ecrire un invariant pour Ville.

construit::Ville -> Zone -> Ville
construit (V zones cit) z = (V (Map.insert (ZoneId (Map.size zones)) z zones) cit)

-- TODO : Question 1.6
pre_construit::Ville -> Zone -> Bool
pre_construit = undefined

-- TODO : Question 1.7
-- Plutôt facile mais je connais pas la syntaxe : vérifier que le map a vu sa taille augmentée de 1 et que la zone
-- est présente dans le map
post_construit::Ville -> Zone -> Bool
post_construit = undefined

-- TODO : ER1 à partir de la Question 1.8