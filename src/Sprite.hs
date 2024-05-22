module Sprite where

import SDL (Renderer, V4(..), Rectangle(..))
import qualified SDL
import Control.Monad.IO.Class (MonadIO)
import Foreign.C.Types (CInt)
import Data.Sequence (Seq(..))
import qualified Data.Sequence as Seq
import SDL.Vect (V2(..), Point(..), V4(..))
import SDL.Video.Renderer (Renderer, Texture, Rectangle(..))
import qualified SDL.Video.Renderer as R
import TextureMap (TextureMap, TextureId)
import qualified TextureMap as TM
import Data.Word (Word8)  -- Correct import for Word8
import qualified SimCity as Sim -- Assuming Zone is defined in Model.hs
import System.IO (hFlush, stdout)


type Area = Rectangle CInt

data Image =
  Image { textureId :: TextureId
        , sourceArea :: Area }

-- | Les lutins sont associés à des images (textures)
createImage :: TextureId -> Area -> Image
createImage txt rct = Image txt rct

data Sprite =
  Sprite { images :: Seq Image
         , current :: Int
         , destArea :: Area }

-- | création d'un lutin "vide"
createEmptySprite :: Sprite
createEmptySprite = Sprite Seq.empty 0 (mkArea 0 0 0 0) 

-- | ajouter une image à un lutin
addImage :: Sprite -> Image -> Sprite
addImage sp@(Sprite { images=is }) img = sp { images = is :|> img }

-- | changer l'image d'un lutin  (par son numéro)
changeImage :: Sprite -> Int -> Sprite
changeImage sp@(Sprite { images = imgs }) new
  | Seq.null imgs = error $ "Cannot change sprite image, no image in sprite"
  | (new < 0) || (new > Seq.length imgs) = error $ "Cannot change sprite image, bad index: " <> (show new)
  | otherwise = sp { current= new }

-- | cycler l'image d'un lutin
cycleImage :: Sprite -> Sprite
cycleImage sp@(Sprite { images = imgs, current = cur }) =
  let new = if cur == Seq.length imgs - 1 then 0 else cur + 1
  in changeImage sp new

-- | une zone rectangulaire
mkArea :: CInt -> CInt -> CInt -> CInt -> Area
mkArea x y w h = Rectangle (P (V2 x y)) (V2 w h)

-- | déplacement d'une zone
moveArea :: Area -> CInt -> CInt -> Area
moveArea rect@(Rectangle _ wh) x y = Rectangle (P (V2 x y)) wh

-- | redimensionnement
resizeArea :: Area -> CInt -> CInt -> Area
resizeArea rect@(Rectangle p _) w h = Rectangle p (V2 w h)

-- | déplacement d'un lutin
moveTo :: Sprite -> CInt -> CInt -> Sprite
moveTo sp@(Sprite { destArea = dest }) x y = sp { destArea = moveArea dest x y }

-- | mise à l'échelle d'un lutin
scale :: Sprite -> CInt -> CInt -> Sprite
scale sp@(Sprite { destArea = dest}) w h = sp { destArea = resizeArea dest w h }

-- | récupération de l'image courante d'un lutin
currentImage :: Sprite -> Image
currentImage (Sprite imgs cur _) = Seq.index imgs cur

-- | échelle par défaut du lutin, en fonction de son image courante
defaultScale :: Sprite -> Sprite
defaultScale sp = case currentImage sp of
                    (Image _ (Rectangle _ (V2 w h))) -> scale sp w h

-- | affichage d'un lutin sur le `renderer` SDL2
displaySprite :: Renderer -> TextureMap -> Sprite -> IO ()
displaySprite rdr tmap sp@(Sprite imgs cur dest) =
  case currentImage sp of
    (Image tid src) -> do
      let txt = TM.fetchTexture tid tmap
      R.copy rdr txt Nothing (Just dest)


zoneColor :: Sim.Zone -> V4 Word8
zoneColor (Sim.ZR _ _) = V4 255 0 0 255     -- Red for residential areas
zoneColor (Sim.ZI _ _) = V4 0 255 0 255     -- Green for industrial areas
zoneColor (Sim.ZC _ _) = V4 0 0 255 255     -- Blue for commercial areas
zoneColor (Sim.Route _) = V4 128 128 128 255 -- Grey for roads
zoneColor (Sim.Eau _) = V4 0 0 128 255      -- Dark blue for water bodies
zoneColor (Sim.Admin _ _) = V4 255 255 0 255 -- Yellow for administrative buildings


-- Function to create and display a colored rectangle on the screen
createColoredSprite :: Renderer -> V4 Word8 -> Rectangle CInt -> IO ()
createColoredSprite renderer color area = do
    SDL.rendererDrawColor renderer SDL.$= color  -- Set the drawing color
    SDL.fillRect renderer (Just area)  -- Draw the filled rectangle