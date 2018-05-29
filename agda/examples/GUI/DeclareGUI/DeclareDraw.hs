

module DeclareGUI.DeclareDraw where

import Data.List (sort, reverse)


import Graphics.Text.TrueType( loadFontFile, Font, PointSize (..) )

--import Text.Peggy as Peggy
--import Text.Peggy.Prim as Prim

--import DeclareGUI.Graphics.Parser (Element (..))
--import qualified DeclareGUI.Graphics.Parser as P

import DeclareGUI.DeclareDefinitions
import DeclareGUI.DeclareHelpers
--import DeclareGUI.Graphics.Parser (Element (..))


import Graphics.Text.TrueType( loadFontFile, Font, PointSize (..) )

import Codec.Picture.Types (PixelRGBA8 (..), Image)

import Graphics.Rasterific (renderDrawing, withTexture, fill, circle, printTextAt, Primitive (..), Drawing (..))
import Graphics.Rasterific.Texture (uniformTexture)
import Graphics.Rasterific.Linear (V2 (..))

import qualified Graphics.Rasterific as R
import Codec.Picture as C
import GHC.Word as W

data Element
  = Rect C.PixelRGBA8 [R.Primitive]
  | SubLabel (V2 Float) String
  deriving Show



element2Drawing :: Font -> Element -> Drawing PixelRGBA8 ()
element2Drawing font (Rect color primitiveList) =
  withTexture (uniformTexture color) $ do
                 fill $ primitiveList

element2Drawing font (SubLabel (V2 x y) str) =
  withTexture (uniformTexture $ PixelRGBA8 0 0 0 255) $
                      printTextAt font (PointSize 16) (V2 x y)
                           str


elements2drawing :: Font -> [Element] -> Drawing PixelRGBA8 ()
elements2drawing font (x : xs) = element2Drawing font x >> elements2drawing font xs
elements2drawing font [] = return ()

drawing2image :: Drawing PixelRGBA8 () -> Image PixelRGBA8
drawing2image = renderDrawing 800 600 backgroundWhite
  where
    backgroundWhite = PixelRGBA8 255 255 255 255


imgFromElements :: Font -> [Element] -> Image PixelRGBA8
imgFromElements font elements = drawing2image $ elements2drawing font elements


-- TODO: add colors
-- add place in center from
-- add label



-- Decleratively
--  -- non-polling
--  -- send messages to the sea "I'm firing" or "I move to the right"
--  -- declare what your requirements/needs are:
--  --  "I need to know when the mouse is withing my bounds"
--  --  "I need to know when one of my sub-buttons fires"

-- ==> How to know the order in which to resolve constraints

toWord :: Int -> W.Word8
toWord x = fromIntegral x --(round x)



-- Later also definie what valid messages are; e.g., creating a name only happens once
--
drawRectangle :: Box -> Color -> Element
drawRectangle (_, ((x, y), (xRight, yRight))) (a, b, c, d) =
  Rect col (R.rectangle (V2 (fromIntegral x) (fromIntegral y)) width height)
  where
    col = (C.PixelRGBA8 (toWord a) (toWord b) (toWord c) (toWord d))
    width = fromIntegral (xRight - x)
    height = fromIntegral (yRight - y)


drawSubLabel :: Int -> Int -> String -> Element
drawSubLabel x y str = SubLabel (V2 (fromIntegral x) (fromIntegral y)) str


drawBox :: Window -> Name -> [Element]
drawBox win@(Window messages _) name 
  | hasSubLabel && hasRectangle =
      [drawRectangle box color] ++ [drawSubLabel x y strSubLabel]
    
  | hasRectangle =
      [drawRectangle box color]

  | hasSubLabel =
      [drawSubLabel x y strSubLabel]
      
  | otherwise = []
        
  where
    relevantMessages =  filter (isOfName name) messages
    --
    labels = filter isSubLabel $ relevantMessages
    rectangles = filter isRectangle $ relevantMessages
    --
    hasSubLabel = length labels >= 1
    hasRectangle = length rectangles >= 1
    --
    (SetRectangle _ _ color) = head $ reverse $ sort $ rectangles
    (SetSubLabel _ _ strSubLabel) = head $ reverse $ sort $ labels
    --
    box@(na, ((x, y), (x2, y2))) = getLatestBox name messages




messages2boxs :: [Message] -> IO [Box]
messages2boxs messages = do
  return $ map (\name -> getLatestBox name messages) (getNames messages)
  --puts $ "latest : " ++ (show $ getLatestBox "hand" messages)
  --res <- sequence $ map (\name -> getLatestBox name messages) names
  --return res


drawWindow :: Window -> [Element]
drawWindow win@(Window messages _) = 
  concat $ map (drawBox win) (getNames messages)
