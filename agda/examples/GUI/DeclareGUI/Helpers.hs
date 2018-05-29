

module DeclareGUI.Helpers where

import Data.List (nub, sort)
import Data.Map.Strict (Map(..), adjust, empty, insert, insertWith, lookup)

import System.Exit (die)

import Codec.Picture.Types (PixelRGBA8 (..))

import DeclareGUI.Definitions

import DeclareGUI.Lib


-- Need to adjust for backspaces
--
getTextboxInptus :: Window -> [String]
getTextboxInptus win = case textboxMap of
  (Just ma) -> map (map2convert ma) allNames
  Nothing -> []
  where
    allNames = getAllTextboxNames win
    --
    map2convert :: (Map Name String) -> Name -> String
    map2convert map name = maybe2String $ Data.Map.Strict.lookup name map
    --
    maybe2String :: Maybe String -> String
    maybe2String Nothing = ""
    maybe2String (Just str) = str
    --
    textboxMap :: Maybe (Map Name String)
    textboxMap = getStringInputOfTextboxes win
    --



-- Window
getStringInputOfTextboxes' :: [Event] -> Name -> Window -> Map Name String -> Map Name String
getStringInputOfTextboxes' ( (Textinput _ newStr)  : xs) textboxName win map =
  getStringInputOfTextboxes' xs textboxName win newMap
  where
    newMap = adjust (++ newStr) textboxName map

getStringInputOfTextboxes' ( (MetaKey _ Backspace)  : xs) textboxName win map =
  getStringInputOfTextboxes' xs textboxName win newMap
  where
    newMap = adjust (++ "\b") textboxName map

getStringInputOfTextboxes' ( (MetaKey _ EnterFocusChange)  : xs) name win map =
  case (findNameAfterWin win name) of
    (Just newName) -> getStringInputOfTextboxes' xs newName win (insertWith (++) newName "" map)
      -- append "" to stored value, so that the key is present in the Map without overwriting a possible existing value stored at that key
    Nothing -> getStringInputOfTextboxes' xs name win map

getStringInputOfTextboxes' ( (TextBoxFocusOn time newFocusName)  : xs) name win map =
  getStringInputOfTextboxes' xs newFocusName win (insertWith (++) newFocusName "" map)

getStringInputOfTextboxes' ( _  : xs) name win map =
  getStringInputOfTextboxes' xs name win map

getStringInputOfTextboxes' [] name win map = map




-- findNameAfter finds the name of the next thing that
-- gets the focus change, such as after a TAB keypress (parmutating to the top textbox at the end)
--
findNameAfter :: [Name] -> [Name] -> Name -> Maybe Name
findNameAfter (n : b : names) xs name | n == name
  = Just b
findNameAfter (n : []) xs name | n == name = safeHead xs    
findNameAfter (n : names) xs name
  = findNameAfter names xs name
findNameAfter _ xs names =
  Nothing

findNameAfterWin :: Window -> Name -> Maybe Name
findNameAfterWin win nam = nextName
  where
    allNames = getAllTextboxNames win
    nextName = findNameAfter allNames allNames nam
  


getStringInputOfTextboxes :: Window -> Maybe (Map Name String)
getStringInputOfTextboxes win@(Window boxes _ events) =
  case (firstTextboxName boxes) of
    (Just name) -> Just $ getStringInputOfTextboxes' events name win (emptyMap name)
    _ -> Nothing
  where
    firstTextboxName :: [Box] -> Maybe Name
    firstTextboxName ( (Box _ nam Textbox _ _) : _) = Just nam
    firstTextboxName ( _ : xs) = firstTextboxName xs
    firstTextboxName _ = Nothing
    --
    emptyMap :: Name -> Map Name String
    emptyMap nam = insert nam "" empty 


  -- TODO TODO
  -- remove backspace charaters!!!
  --map removeCharactersBeforeBackspace (getStringInputOfTextboxes' events "" [])


-- Textbox names
--
getAllTextboxNames :: Window -> [Name]
getAllTextboxNames (Window boxes _ _) = getAllTextboxNames' boxes
  where
    getAllTextboxNames' :: [Box] -> [Name]
    getAllTextboxNames' ((Box _ nam Textbox _ _) : xs) = nam : getAllTextboxNames' xs
    getAllTextboxNames' (_ : xs) = getAllTextboxNames' xs
    getAllTextboxNames' [] = []




-- Number of textboxes of a frame for checks
--
isTextboxComponent :: Component -> Bool
isTextboxComponent (TextboxComponent _) = True
isTextboxComponent _ = False

numberOfTextboxes :: Frame -> Int
numberOfTextboxes fr = length $ filter isTextboxComponent fr



color2PixelRGBA8 :: Color -> PixelRGBA8
color2PixelRGBA8 (a, b, c, d) = PixelRGBA8 (fromIntegral a) (fromIntegral b) (fromIntegral c) (fromIntegral d)


isButtonComp :: Component -> Bool
isButtonComp (ButtonComponent _) = True
isButtonComp _ = False

getBoxName :: Box -> Name
getBoxName (Box _ name _ _ _) = name

filterBoxesSameName :: Name -> [Box] -> [Box]
filterBoxesSameName name ( b@(Box _ bxName _ _ _) : xs ) | bxName == name = b : filterBoxesSameName name xs
filterBoxesSameName name ( _ : xs ) = filterBoxesSameName name xs
filterBoxesSameName name [] = []


getMaxTimeEvents :: [Event] -> Time
getMaxTimeEvents events = maximum $ map event2Time events
  where
    event2Time :: Event -> Time
    event2Time (LeftButtonReleased t _) = t
    event2Time (Textinput t _) = t
    event2Time (MetaKey t _) = t
    event2Time (TextBoxFocusOn t _) = t


getMaxTimeBoxes :: [Box] -> Time
getMaxTimeBoxes boxes = maximum $ map box2Time boxes
  where
    box2Time :: Box -> Time
    box2Time (Box t _ _ _ _) = t

-- box name is not present leads to failed pattern match
-- (this version returns the latest time of a boxname)
getBoxByName :: Name -> [Box] -> Maybe Box
getBoxByName name boxes@( b@(Box t bxName _ _ _) : _) | ((bxName == name) && (t == maxT)) = Just b
  where
    maxT = getMaxTimeBoxes $ filterBoxesSameName name boxes
getBoxByName name (_ : xs) = getBoxByName name xs
getBoxByName _ [] = Nothing



--
-- Predicates
--
wasBoxClickedNow :: Name -> Window -> Time -> Bool
wasBoxClickedNow boxName window@(Window boxes constraints events) time =
  any (==True) hasBeenClicked
  where
    box :: Box
    Just box = getBoxByName boxName boxes
    --
    isNowClickedBox :: Event -> Bool
    isNowClickedBox (LeftButtonReleased t point) =  (t == time) && (pointOverlapsBox box point)
    isNowClickedBox _ = False
    --
    hasBeenClicked :: [Bool]
    hasBeenClicked = map isNowClickedBox events
    

pointOverlapsBox :: Box -> Point -> Bool
pointOverlapsBox (Box _ _ _ ((xLeft, yLeft), (xRight, yRight)) _) (x, y)
 | (inBetween x xLeft xRight) &&
   (inBetween y yLeft yRight) = True
 | otherwise = False



