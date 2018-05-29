




module DeclareGUI.CreateComponents where

import Graphics.Text.TrueType(PointSize (..) )

import DeclareGUI.Definitions

import qualified DeclareGUI.Style as Style

import DeclareGUI.Lib

-- Note: vlist is the list of the vertical spaces

-- Position of a Element
--
elementPosition :: [Int] -> Int -> Int -> Position
elementPosition vlist n heightElem = boxRect
  where
    marginLeft = Style.leftMargin
    width = Style.widthElements
    height = heightElem
    
    vspa = case (lookupList n vlist) of
      Just x -> x
      Nothing -> n * 70
    
    --
    -- This needs to be refined:
    boxRect = ((marginLeft, (height + vspa)),
               (marginLeft + width, (height + vspa) + height))


-- Textlabel of an element
--
subLabelPosition :: [Int] -> Int -> Int -> Int -> Position
subLabelPosition vlist n indet vOffset = labelRect
  where
    ((x, y), (x2, y2)) = elementPosition vlist n Style.heightElements
    --
    labelRect = ((x + indet, y + Style.subLabelMovedDown + vOffset),
                 (x2, y2))


makeElement :: [Int] -> Int -> String -> Time -> Color -> Int -> Int -> Int -> Color -> [Box]
makeElement vlist n labelStr t rectColor labelIndent vOffset fontSize fontColor =
  rectBox : labelBox : []
  where
    strN = show n
    rectangleName = "Rectangle" ++ strN
    labelName = "SubLabel" ++ strN
    --
    rectPosition = elementPosition vlist n Style.heightElements
    labelPosition = subLabelPosition vlist n labelIndent vOffset
    --
    rectBox = Box t rectangleName (Rectangle rectColor) rectPosition []
    labelBox = Box t labelName (SubLabel labelStr fontColor (PointSize (fromIntegral fontSize))) labelPosition []


makeButton :: [Int] -> Int -> String -> Time -> [Box]
makeButton vlist n labelStr t =
  box : makeElement vlist n labelStr t Style.colorButton
          Style.buttonLabelIndent (-3) Style.fontSizeButton Style.white
  where
    buttonName = "Button" ++ show n
    --
    rectPosition = elementPosition vlist n Style.heightElements --Style.heightButton [click prob]
    box = Box t buttonName Button rectPosition (subBoxNames n)


makeTextbox :: [Int] -> Int -> String -> Time -> [Box]
makeTextbox vlist n labelStr t =
  box : makeElement vlist n labelStr t Style.colorTextbox Style.textboxLabelIndent
        (-2) Style.fontSizeTextbox Style.black
  where
    textboxName = "Textbox" ++ show n
    --
    rectPosition = elementPosition vlist n Style.heightElements
    box = Box t textboxName Textbox rectPosition (subBoxNames n)


makeLabel :: [Int] -> Int -> String -> Time -> [Box]
makeLabel vlist n labelStr t =
  box : makeElement vlist n labelStr t Style.colorLabel Style.labelLabelIndent 0
        Style.fontSizeLabel Style.black
  where
    labelName = "Label" ++ show n
    --
    rectPosition = elementPosition vlist n Style.heightElements
    box = Box t labelName Label rectPosition (subBoxNames n)



subBoxNames :: Int -> [String]
subBoxNames n = rectangleName : subLabelName : []
  where
    strN = show n
    rectangleName = "Rectangle" ++ strN
    subLabelName = "SubLabel" ++ strN











