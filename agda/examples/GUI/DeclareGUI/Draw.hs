

module DeclareGUI.Draw where

import Data.List (sort, reverse)

import DeclareGUI.Definitions
import DeclareGUI.Helpers
import DeclareGUI.Lib

import qualified Data.Map.Strict as Map 

import Graphics.Text.TrueType( loadFontFile, Font, PointSize (..) )

import Graphics.Rasterific (renderDrawing, withTexture, fill, circle, printTextAt, Primitive (..), Drawing (..))

import Graphics.Rasterific.Texture (uniformTexture)
import Graphics.Rasterific.Linear (V2 (..))

import qualified Graphics.Rasterific as R
import Codec.Picture as C
import GHC.Word as W

import DeclareGUI.Helpers (getBoxName)

import DeclareGUI.Lib

import DeclareGUI.Definitions
import DeclareGUI.FrameToWindow (frame2window)


import qualified SDL as SDL
import Codec.Picture.Types (Image, PixelRGBA8 (..))
import Graphics.Rasterific (renderDrawing)

import Graphics.Text.TrueType( loadFontFile, Font, PointSize (..) )

import DeclareGUI.SDLLIB

import DeclareGUI.Definitions
import DeclareGUI.Style

import qualified SDL as SDL

import qualified DeclareGUI.Style as Style

--
-- Render
--
renderWindow :: Font -> Window -> SDL.Renderer -> IO ()
renderWindow font window@(Window boxes constraints events) renderer = do
  renderImg renderer pic 
  where
    pic :: Image PixelRGBA8
    pic = renderDrawing Style.screenHeight Style.screenWidth windowBackgroundColor $ drawWindow font window


getPartentTextboxFromSubLabel :: Name -> [Box] -> Maybe Box
getPartentTextboxFromSubLabel name ( bx@(Box _ parentName _ _ subNames) : xs ) | elem name subNames = Just bx

getPartentTextboxFromSubLabel name ( _ : xs ) = getPartentTextboxFromSubLabel name xs
getPartentTextboxFromSubLabel name [] = Nothing


getPartentTextboxFromSubLabelName :: Name -> [Box] -> Maybe Name
getPartentTextboxFromSubLabelName name boxes =
  case (getPartentTextboxFromSubLabel name boxes) of
    Just (Box _ parentName _ _ _) -> Just parentName
    Nothing -> Nothing





drawBoxesByNames :: [Name] -> [Box] -> Font -> Window -> Drawing PixelRGBA8 ()
drawBoxesByNames (name : xs) boxes font win =
  case (getBoxByName name boxes) of
    (Just b) -> drawBox font win b >> drawBoxesByNames xs boxes font win
    Nothing -> return ()
drawBoxesByNames [] _ _ _ = return ()


drawBox :: Font -> Window -> Box -> Drawing PixelRGBA8 ()
-- Rectangle
--
drawBox _ (Window boxes _ _) (Box _ _ (Rectangle (a, b, c, d)) ((x, y), (xRight, yRight)) []) =
  withTexture (uniformTexture col) $ do
                 fill $ primitiveList    
  where    
    width = fromIntegral (xRight - x)
    height = fromIntegral (yRight - y)
    --
    col = (C.PixelRGBA8 (toWord a) (toWord b) (toWord c) (toWord d))
    primitiveList = (R.rectangle (V2 (fromIntegral x) (fromIntegral y)) width height)

-- SubLabel
--
drawBox font win@(Window boxes _ events) (Box _ labelName (SubLabel subLabelStr color pointSz) ((x, y), (xRight, yRight)) [])  =
  mappend
  (withTexture (uniformTexture $ color2PixelRGBA8 color) $
                      printTextAt font pointSz (V2 (fromIntegral x) (fromIntegral y))
                      fullLabelStr)
  bounding
  where
    textboxMap :: Maybe (Map.Map Name String)
    textboxMap = getStringInputOfTextboxes win


    -- TODO: needs to remove the backspaces
    --
    textboxString :: String
    textboxString = case textboxMap of
      Just m -> case (Map.lookup parentBoxName m) of
        (Just str) -> str
        Nothing -> (if Style.debug then "[nothing: lookup parentBoxName]" else "")

      Nothing -> (if Style.debug then "[nothing: textboxMap]" else "")
    --
    fullLabelStr = case isTextBox of
      True -> (removeCharactersBeforeBackspace textboxString)
      False -> subLabelStr
    --
    Just parentBoxName = getPartentTextboxFromSubLabelName labelName boxes
    Just parentBox@(Box _ _ parentBoxType _ _) = getPartentTextboxFromSubLabel labelName boxes
    isTextBox = (parentBoxType == Textbox)
    --
    width = fromIntegral (xRight - x)
    height = fromIntegral (yRight - y)
    --
    boundingBox = (withTexture (uniformTexture (color2PixelRGBA8 (0, 0, 0, 255))) $ do
                 (R.stroke 0.3 R.JoinRound (R.CapRound, R.CapRound)) $ boundingPrimitives )
    bounding = if (Style.drawBoundingBoxes) then boundingBox else mempty
    --
    boundingPrimitives = (R.rectangle (V2 (fromIntegral x) (fromIntegral y)) width height)


    {-
    -- from old code drawing the Textbox string/label

    allTextInputs = getStringInputOfTextboxes events
    
    textboxInput :: String
    textboxInput = case (getTextboxNumber boxes parentBoxName) of
      Just num -> case(lookupList num allTextInputs) of
        Just str -> str
        Nothing -> ""
        
      Nothing -> ""
    fullLabelStr = case isTextBox of
      True -> (subLabelStr ++ textboxInput)
      False -> subLabelStr
    -}
                 

--
-- (note: the button/textbox/label box itself if not drawn; it acts like a container)

-- Button
--
drawBox font win@(Window boxes _ _) (Box _ _ Button _ names) = drawBoxesByNames names boxes font win

-- Textbox
--
drawBox font win@(Window boxes _ _) (Box _ _ Textbox _ names) = drawBoxesByNames names boxes font win

-- Textbox
--
drawBox font win@(Window boxes _ _) (Box _ _ Label _ names) = drawBoxesByNames names boxes font win


drawBox font win@(Window boxes _ _) box  = 
  withTexture (uniformTexture $ PixelRGBA8 0 0 0 255) $
                      printTextAt font (PointSize 6) (V2 (fromIntegral 50) (fromIntegral 50))
                      (show boxes)


drawWindow :: Font -> Window -> Drawing PixelRGBA8 ()
drawWindow font win@(Window boxes _ _) =
  mappend
    (mconcat $ map (drawBox font win) boxes)
    bouding
      
  where    
    mconcat = foldr mappend mempty
    --
    primitiveList = (R.rectangle (V2 (fromIntegral 0) (fromIntegral 0)) 48 78)
    boundingBoxAtTop = (withTexture (uniformTexture (color2PixelRGBA8 (0, 0, 0, 255))) $ do
                 (R.stroke 1.0 R.JoinRound (R.CapRound, R.CapRound)) $ primitiveList )
                       
    bouding = if (Style.drawBoundingBoxes) then boundingBoxAtTop else mempty





-- ERROR
--
{-drawBox font win@(Window boxes _ events) (Box a b (SubLabel x _ _) c d)  = 
  withTexture (uniformTexture $ PixelRGBA8 0 0 0 255) $
                      printTextAt font (PointSize 6) (V2 (fromIntegral 50) (fromIntegral 50))
                      ("ERROR sublabel fList: " ++ (show fList))-}
