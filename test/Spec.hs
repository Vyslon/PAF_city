module Spec where

import Test.QuickCheck
import System.Random (Random)
import SimCity
import Data.Maybe

--import Distribution.Compat.Prelude (undefined)

-- On part du principe que les abscisses vont de 0 à 640 et les ordonnées de 0 à 480
main :: IO ()
main = putStrLn "Test suite not yet implemented"

-- Viens du TME8
samples :: Int -> Gen a -> IO [a]
samples n gen = do
  l <- sample' gen
  return $ take n l

genCoord :: Int -> Int -> Gen Coord
genCoord maxX maxY = C <$> choose (0, maxX) <*> choose (0, maxY)

-- Générateur de segment horizontal
genHSegment :: Gen Forme
genHSegment = do
    longueur <- choose (1, 40)
    coord <- genCoord (640 - longueur) (480)
    return $ HSegment coord longueur

-- Générateur de segment vertical
genVSegment :: Gen Forme
genVSegment = do
    hauteur <- choose (1, 40)
    coord <- genCoord (640) (480 - hauteur)
    return $ VSegment coord hauteur

-- Générateur de rectangle
genRectangle :: Gen Forme
genRectangle = do
    largeur <- choose (1, 40)
    hauteur <- choose (1, 40)
    coord <- genCoord (640 - largeur) (480 - hauteur)
    return $ Rectangle coord largeur hauteur

-- Générateur de forme
genForme :: Gen Forme
genForme = frequency [(30, genHSegment), (30, genVSegment), (40, genRectangle)]

genArgent :: Gen Int
genArgent = frequency [ (84, choose (500, 10000)),
                        (10, choose (10001, 100000)),
                        (5, choose (100001, 1000000)),
                        (1, choose (1000001, 1000000000)) ] 

-- Générateur d'identifiant de citoyen
genCitId :: Gen CitId
genCitId = CitId <$> vectorOf 10 (choose ('a', 'z'))

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
    (25, pure (SeDeplaceVers coord))
    ]

-- Générateur d'émigrant
-- Entrée : coordonnées de la sortie de la ville
genEmigrant :: Coord -> Gen Citoyen
genEmigrant sortieVille = do
    coord <- (genCoord 640 480)
    return $ Emigrant coord (SeDeplaceVers sortieVille)

-- Générateur d'immigrant
-- Entrée : coordonnées du logement disponible le plus proche
genImmigrant :: Coord -> Gen Citoyen
genImmigrant coordLogementDisponible = do
    coord <- (genCoord 640 480)
    argent <- genArgent
    fatigue <- (choose (2700, 3600))
    faim <- (choose (540, 720))
    return $ Immigrant coord (argent, fatigue, faim) (SeDeplaceVers coordLogementDisponible)

-- Générateur de citoyen
-- Entrée :
--      * Id de la résidence dans laquelle le citoyen vit
--      * Id de l'entreprise dans laquelle le citoyen travaille
--      * Id du magasin dans lequel le citoyen fait ses courses
genHabitant :: BatId -> Maybe BatId -> Maybe BatId -> Gen Citoyen
genHabitant residence entreprise magasin = do
    -- Définir une coordonnée par défaut si les BatIds nécessaires sont absents
    let coordResidence = coordDepuisId residence
    occupation <- genOccupation entreprise magasin coordResidence
    argent <- genArgent
    fatigue <- choose (2700, 3600)
    faim <- choose (540, 720)
    coord <- case occupation of
        Travaille -> return $ coordDepuisId (fromMaybe residence entreprise)
        Dors -> return $ coordDepuisId residence
        FaisLesCourses -> return $ coordDepuisId (fromMaybe residence magasin)
        SeDeplaceVers _ -> genCoord 640 480
        _ -> return $ coordDepuisId residence
    return $ Habitant coord (argent, fatigue, faim) (residence, entreprise, magasin) occupation
