

module DeclareGUI.Style where

import DeclareGUI.Definitions
import Codec.Picture.Types (PixelRGBA8 (..))


-- Debug / Print output
--
debug :: Bool
debug = False

-- Screen size
--
screenHeight :: Int 
screenHeight = 800

screenWidth :: Int 
screenWidth = 600


-- Colors
--
white :: Color
white = (255, 255, 255, 255)

grey :: PixelRGBA8
grey = PixelRGBA8 220 220 220 255

black :: Color
black = (0, 0, 0, 255)

-- Window
--
winWidth :: Int
winWidth = 400

winHeight :: Int
winHeight = 500

windowBackgroundColor :: PixelRGBA8
windowBackgroundColor = grey
    

-- Background color of GUI elements:
--
colorButton :: Color
colorButton = (95, 95, 95, 255)

colorTextbox :: Color
colorTextbox = white --(200, 200, 200, 255)
  where


colorLabel :: Color
colorLabel = transparent
  where
    transparent = (255, 255, 255, 0)


-- Elements Position
--
subLabelMovedDown :: Int
subLabelMovedDown = 30

-- Margin and sizes
--
leftMargin :: Int
leftMargin = 48

widthElements :: Int
widthElements = 300

heightElements :: Int
heightElements = 40

-- looks a slightly better, but left out because it's only 4 px
--heightButton :: Int
--heightButton = 36

-- Font sizes
--
fontSizeButton :: Int
fontSizeButton = 14

fontSizeTextbox :: Int
fontSizeTextbox = 12

fontSizeLabel :: Int
fontSizeLabel = 12


-- Label Indents
--
labelLabelIndent :: Int
labelLabelIndent = 0

textboxLabelIndent :: Int
textboxLabelIndent = 10

buttonLabelIndent :: Int
buttonLabelIndent = 100

-- For debugging
--
drawBoundingBoxes :: Bool
drawBoundingBoxes = False


textboxVerticalOffset :: Int
textboxVerticalOffset = 40

labelVerticalOffset :: Int
labelVerticalOffset = 90

buttonAdditionalVerticalOffset :: Int
buttonAdditionalVerticalOffset = 23

{-
vspace :: Int -> Int
vspace 0 = 0    -- unused
vspace 1 = 8   -- 'age:'
vspace 2 = 53   -- '65'
vspace 3 = 108  -- 'weight:'
vspace 4 = 148  -- '70'
vspace 5 = 231
vspace nthPositionInGUI = 2000
-}
