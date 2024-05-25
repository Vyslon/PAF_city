
import Test.Hspec
import Test.QuickCheck
import Generators as GT
import SimCity

main :: IO ()
main = hspec $ do
    testAppartient
    testAdjacent
    testCollisionManuelle
    GT.genVilleOk


testAppartient :: Spec
testAppartient = describe "appartient" $ do
  it "should return True for coordinates that belong to HSegment" $ property $
    \(Positive x) (Positive y) (Positive longueur) (NonNegative dx) -> 
      (dx <= longueur) ==> 
      (appartient (C (x + dx) y) (HSegment (C x y) longueur) == True)
  
  it "should return False for coordinates that do not belong to HSegment" $ property $
    \(Positive x) (Positive y) (Positive longueur) (NonNegative dx) -> 
      (dx > longueur) ==> 
      (appartient (C (x + dx) y) (HSegment (C x y) longueur) == False)

  it "should return True for coordinates that belong to VSegment" $ property $
    \(Positive x) (Positive y) (Positive hauteur) (NonNegative dy) -> 
      (dy <= hauteur) ==> 
      (appartient (C x (y + dy)) (VSegment (C x y) hauteur) == True)

  it "should return False for coordinates that do not belong to VSegment" $ property $
    \(Positive x) (Positive y) (Positive hauteur) (NonNegative dy) -> 
      (dy > hauteur) ==> 
      (appartient (C x (y + dy)) (VSegment (C x y) hauteur) == False)

  it "should return True for coordinates that belong to Rectangle" $ property $
    \(Positive x) (Positive y) (Positive largeur) (Positive hauteur) (NonNegative dx) (NonNegative dy) -> 
      (dx <= largeur && dy <= hauteur) ==> 
      (appartient (C (x + dx) (y + dy)) (Rectangle (C x y) largeur hauteur) == True)

  it "should return False for coordinates that do not belong to Rectangle" $ property $
    \(Positive x) (Positive y) (Positive largeur) (Positive hauteur) (NonNegative dx) (NonNegative dy) -> 
      (dx > largeur || dy > hauteur) ==> 
      (appartient (C (x + dx) (y + dy)) (Rectangle (C x y) largeur hauteur) == False)

testAdjacent :: Spec
testAdjacent = describe "adjacent" $ do
  -- Tests for HSegment
  it "should return True for coordinates that are adjacent to HSegment" $ property $
    \(Positive x) (Positive y) (Positive longueur) dx dy -> 
      (dx <= longueur + 30 && dx >= -30 && dy <= 30 && dy >= -30) ==> 
      adjacent (C (x + dx) (y + dy)) (HSegment (C x y) longueur)

  it "should return False for coordinates that are not adjacent to HSegment" $ property $
    \(Positive x) (Positive y) (Positive longueur) dx dy -> 
      (dx > longueur + 30 || dx < -30 || dy > 30 || dy < -30) ==> 
      not (adjacent (C (x + dx) (y + dy)) (HSegment (C x y) longueur))

  -- Tests for VSegment
  it "should return True for coordinates that are adjacent to VSegment" $ property $
    \(Positive x) (Positive y) (Positive hauteur) dx dy -> 
      (dx <= 30 && dx >= -30 && dy <= 30 && dy >= -30 - hauteur) ==> 
      adjacent (C (x + dx) (y + dy)) (VSegment (C x y) hauteur)

  it "should return False for coordinates that are not adjacent to VSegment" $ property $
    \(Positive x) (Positive y) (Positive hauteur) dx dy -> 
      (dx > 30 || dx < -30 || dy > 30 || dy < -30 - hauteur) ==> 
      not (adjacent (C (x + dx) (y + dy)) (VSegment (C x y) hauteur))

  -- Tests for Rectangle
  it "should return True for coordinates that are adjacent to Rectangle" $ property $
    \(Positive x) (Positive y) (Positive largeur) (Positive hauteur) dx dy -> 
      let cx = x + dx
          cy = y + dy
          adjHori = abs (cx - (x - 30)) <= 30 || abs (cx - (x + largeur + 30)) <= 30
          adjVert = abs (cy - (y - 30 - hauteur)) <= 30 || abs (cy - (y + 30)) <= 30
      in (adjHori && (cy >= (y - 30 - hauteur) && cy <= (y + 30))) || 
         (adjVert && (cx >= (x - 30) && cx <= (x + largeur + 30))) ==> 
         adjacent (C cx cy) (Rectangle (C x y) largeur hauteur)

  it "should return False for coordinates that are not adjacent to Rectangle" $ property $
    \(Positive x) (Positive y) (Positive largeur) (Positive hauteur) dx dy -> 
      let cx = x + dx
          cy = y + dy
          adjHori = abs (cx - (x - 30)) > 30 && abs (cx - (x + largeur + 30)) > 30
          adjVert = abs (cy - (y - 30 - hauteur)) > 30 && abs (cy - (y + 30)) > 30
      in (adjHori || (cy < (y - 30) || cy > (y + hauteur + 30))) && 
         (adjVert || (cx < (x - 30) || cx > (x + largeur + 30))) ==> 
         not (adjacent (C cx cy) (Rectangle (C x y) largeur hauteur))

testCollisionManuelle :: Spec
testCollisionManuelle = describe "collisionManuelle" $ do
  -- Tests for collision between HSegments
  it "should return True for colliding HSegments" $ property $
    \(Positive x1) (Positive y1) (Positive length1) (Positive length2) ->
      let x2 = x1 + length1 `div` 2  -- Ensure that x2 is within the range of x1 + length1
          forme1 = HSegment (C x1 y1) length1
          forme2 = HSegment (C x2 y1) length2  -- Same y-coordinate to ensure collision
      in collisionManuelle forme1 forme2

  it "should return False for non-colliding HSegments" $ property $
    \(Positive x1) (Positive y1) (Positive length1) (Positive x2) (Positive y2) (Positive length2) ->
      let forme1 = HSegment (C x1 y1) length1
          forme2 = HSegment (C x2 y2) length2
      in (y1 /= y2 || x1 + length1 < x2 || x2 + length2 < x1) ==> not (collisionManuelle forme1 forme2)

  it "should return True for colliding VSegments" $ property $
    \(Positive x1) (Positive y1) (Positive height1) (Positive height2) ->
      let y2 = y1 - height1 `div` 2  -- Ensure that y2 is within the range of y1 - height1
          forme1 = VSegment (C x1 y1) height1
          forme2 = VSegment (C x1 y2) height2  -- Same x-coordinate to ensure collision
      in collisionManuelle forme1 forme2

  it "should return False for non-colliding VSegments" $ property $
    \(Positive x1) (Positive y1) (Positive height1) (Positive x2) (Positive y2) (Positive height2) ->
      let forme1 = VSegment (C x1 y1) height1
          forme2 = VSegment (C x2 y2) height2
      in (x1 /= x2 || y1 - height1 > y2 || y2 - height2 > y1) ==> not (collisionManuelle forme1 forme2)

  -- Tests for collision between Rectangles
  it "should return True for colliding Rectangles" $ property $
    \(Positive x1) (Positive y1) (Positive width1) (Positive height1) (Positive x2) (Positive y2) (Positive width2) (Positive height2) ->
      let forme1 = Rectangle (C x1 y1) width1 height1
          forme2 = Rectangle (C x2 y2) width2 height2
      in (x1 <= x2 + width2) && (x2 <= x1 + width1) && (y1 >= y2 - height2) && (y2 >= y1 - height1) ==> collisionManuelle forme1 forme2

    
  it "should return False for non-colliding Rectangles" $ property $
    \(Positive x1) (Positive y1) (Positive width1) (Positive height1) (Positive x2) (Positive y2) (Positive width2) (Positive height2) ->
      let forme1 = Rectangle (C x1 y1) width1 height1
          forme2 = Rectangle (C x2 y2) width2 height2
      in (x1 > x2 + width2 || x2 > x1 + width1 || y1 < y2 - height2 || y2 < y1 - height1) ==> not (collisionManuelle forme1 forme2)
