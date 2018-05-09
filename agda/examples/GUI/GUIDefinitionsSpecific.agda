module GUI.GUIDefinitionsSpecific where

open import Data.String
open import Data.List
open import Data.Fin hiding (_+_)
open import Data.Sum
open import Data.Unit
open import Data.Empty
open import Data.Product
open import Data.Bool
open import Data.Nat
open import Data.Maybe.Base

open import Data.Product
open import Function

open import NativeIO
open import Size
open import SizedIO.Base using (IOInterface; Command; Response; IO; return)
open import SizedIO.Console using (consoleI; translateIOConsole)
open import StateSizedIO.GUI.BaseStateDependent
open import heap.libraryVec
open import GUI.GUIDefinitionsBase





bool2Nat : Bool → ℕ
bool2Nat true = 1
bool2Nat false = 0

guiEl2NrButtons : (cmpStruc : CompStruc)
                (c : Components cmpStruc)
                (g : GUIel cmpStruc c)
                → ℕ
guiEl2NrButtons cmpStruc c (createCmp x) = bool2Nat (hasMethod cmpStruc c)
guiEl2NrButtons cmpStruc c (addCmp g i g') =
       guiEl2NrButtons cmpStruc (subComp cmpStruc c i) g' +
       guiEl2NrButtons cmpStruc c g

guiEl2NrTextboxes : (cmpStruc : CompStruc)
                    (c : Components cmpStruc)
                    (g : GUIel cmpStruc c)
                   → ℕ
guiEl2NrTextboxes cmpStruc c (createCmp x) = bool2Nat (returnsStr cmpStruc c)
guiEl2NrTextboxes cmpStruc c (addCmp g i g') =
       guiEl2NrTextboxes cmpStruc (subComp cmpStruc c i) g' +
       guiEl2NrTextboxes cmpStruc c g



guiEl2Props : (cmpStruc : CompStruc)
              (c : Components cmpStruc)
              (g : GUIel cmpStruc c)
              → Set
guiEl2Props cmpStruc c (createCmp x) = Properties cmpStruc c
guiEl2Props cmpStruc c (addCmp g i g') =
       guiEl2Props cmpStruc c g ×
       guiEl2Props cmpStruc (subComp cmpStruc c i) g'


-- Could name that module and open it with our default 2 parameters!?
--

module _  (cmpStruc : CompStruc)(c : Components cmpStruc) where
  GUIState : Set
  GUIState = GUIel cmpStruc c

  GUIElMethod : GUIState → Set
  GUIElMethod g = Fin (guiEl2NrButtons cmpStruc c g) ×  Tuple String (guiEl2NrTextboxes cmpStruc c g)






  GUIElResult : (g : GUIel cmpStruc c) → (GUIElMethod g) → Set
  GUIElResult g m = GUIel cmpStruc c


  nextGUI : (g : GUIel cmpStruc c) → (m : GUIElMethod g) → (r : GUIElResult g m) → GUIState
  nextGUI g m r = r

--\GUIDefinitions
  GUIInterface : Interfaceˢ
  GUIInterface .Stateˢ  = GUIState
  GUIInterface .Methodˢ = GUIElMethod
  GUIInterface .Resultˢ = GUIElResult
  GUIInterface .nextˢ = nextGUI


  -- Old Paper this is called "HandlerObject":
  --
  GUIObj : ∀{i} → GUIel cmpStruc c → Set
  GUIObj {i} gui = IOObjectˢ consoleI GUIInterface i gui



record GUIgen {i : Size}(cmpStruc : CompStruc)(c : Components cmpStruc) : Set where
  constructor guic
  field
    gui : GUIel cmpStruc c
    obj : GUIObj cmpStruc c {i} gui

open GUIgen public

GUIgenMethod : (cmpStruc : CompStruc)(c : Components cmpStruc) → GUIgen cmpStruc c → Set
GUIgenMethod cmpStruc c g = GUIElMethod cmpStruc c (g .gui)

GUIgenResult : (cmpStruc : CompStruc)(c : Components cmpStruc)(g : GUIgen cmpStruc c) → (GUIgenMethod cmpStruc c g) → Set
GUIgenResult cmpStruc c g m = GUIElResult cmpStruc c (g .gui) m

Frame : Set
Frame = GUIel frameCmpStruc frame

FrameObj : {i : Size} → Frame → Set
FrameObj {i} = GUIObj frameCmpStruc frame {i}

FrameMethod : Frame → Set
FrameMethod  = GUIElMethod frameCmpStruc frame

-- in GUIDefinitionsGeneric this is derived from GUI
-- we have here a specialised version so that it is easier to presentx

-- \GUIDefinitions
record GUI : Set where 
  constructor guic
  field 
    gui : Frame
    obj : FrameObj gui

open GUI public

GUIMethod : GUI → Set
GUIMethod g = GUIElMethod frameCmpStruc frame (g .gui)



-- "HandlerIOType" is now called "GUIObjIOType" definied in GUIModelRasterificSimplified
--

-- translate GUI into the sigma type
-- which is required as a return type
handleGUI2pair :  GUI →  Σ[ g ∈ Frame ]  (FrameObj g)
handleGUI2pair x = x .gui , x .obj

returnGUI : (g : GUI) → IO consoleI ∞ (Σ[ g ∈ Frame ]
                                                       (FrameObj g))
returnGUI g = return (handleGUI2pair g)


FrameResult : (g : Frame) → (FrameMethod g) → Set
FrameResult =  GUIElResult frameCmpStruc frame

GUIResult : (g : GUI) → (GUIMethod g) → Set
GUIResult g  m =  FrameResult (g .gui) m

nextFrame : (g : Frame) → (m : FrameMethod g) → (r : FrameResult g m) → Frame
nextFrame = nextGUI frameCmpStruc frame

nextGUIFrame : (g : GUI) → (m : GUIMethod g) → (r : GUIResult g m) → Frame
nextGUIFrame g = nextFrame (g .gui)



-- INFO: properties are part of add Components  !!
--
{- ####
###=-}
