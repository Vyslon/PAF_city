module SimCity where


data Coord = C {cx :: Int, cy :: Int} deriving (Show , Eq)

data Forme = HSegment Coord Int
    | VSegment Coord Int
    | Rectangle Coord Int Int

-- (nord, sud, ouest, est)
limites::Forme -> (Int, Int, Int, Int)
limites (HSegment (C x y) longueur) = (y, y, x, x + longueur)
limites (VSegment (C x y) longueur) = (y, y - longueur, x, x)
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
    | x1 == (x2 - 1) = 

-- Consigne : Dans les questions suivantes, on ne fera plus de supposition sur les constructeurs de Forme
-- (par exemple, on n’´ecrira plus HSegment), on utilisera uniquement les trois fonctions pr´ec´edentes. (Ainsi,
-- si on ajoute de nouveaux constructeurs `a Forme, seules les trois fonctions pr´ec´edentes devront ˆetre mises `a
-- jour).