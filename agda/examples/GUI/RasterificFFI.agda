module GUI.RasterificFFI where

open import Data.String.Base
open import Data.List.Base
open import Data.Unit.Base
open import Data.Maybe
open import Data.Nat
open import Data.Product

open import NativeIO renaming (NativeIO to IO;
                               nativeReturn to return;
                               _native>>=_ to _>>=_;
                               _native>>_ to _>>_)


open import  GUI.GUIDefinitionsBase
open import  GUI.GUIDefinitions

{-# FOREIGN GHC import qualified DeclareGUI.MainGUI              as M #-}
{-# FOREIGN GHC import qualified DeclareGUI.GUI                  as G #-}
{-# FOREIGN GHC import qualified DeclareGUI.Definitions          as D #-}
{-# FOREIGN GHC import qualified DeclareGUI.SDLLIB               as SDL #-}
{-# FOREIGN GHC import qualified DeclareGUI.RenderEvents         as R #-}





--
-- Types
--
postulate
  SDLWindow : Set     -- Window to render on
  FFIFrame  : Set     -- Frame definition of Buttons, etc.
  FFIComponent : Set  -- Components such as Buttons, etc.

{-# COMPILE GHC FFIFrame       = type D.Frame #-}
{-# COMPILE GHC SDLWindow      = type SDL.SDLWindow  #-}
{-# COMPILE GHC FFIComponent   = type D.Component #-}

--
-- Components and conversion
--
FFIComponents : Set
FFIComponents = List FFIComponent



postulate
--  components2frame : FFIComponents → FFIFrame
  ffiButton  : String → FFIComponent
  ffiLabel    : String → FFIComponent
  ffiTextbox  : String → FFIComponent

--{-# COMPILE GHC components2frame      = D.frame2gui #-}
{-# COMPILE GHC ffiButton         = (\x -> D.ButtonComponent (Data.Text.unpack x))  #-}
{-# COMPILE GHC ffiLabel          = (\x -> D.LabelComponent (Data.Text.unpack x))   #-}
{-# COMPILE GHC ffiTextbox        = (\x -> D.TextboxComponent (Data.Text.unpack x)) #-}

frame2FFI : Frame → FFIComponents
frame2FFI (createCmp tt) = []
frame2FFI (addCmp x buttonIdx (createCmp str)) = ffiButton str ∷ frame2FFI x
frame2FFI (addCmp x labelIdx (createCmp str)) = ffiLabel str ∷ frame2FFI x
frame2FFI (addCmp x textboxIdx (createCmp str)) = ffiTextbox str ∷ frame2FFI x
frame2FFI (addCmp _ buttonIdx (addCmp _ () _))
frame2FFI (addCmp _ labelIdx (addCmp _ () _))
frame2FFI (addCmp _ textboxIdx (addCmp _ () _))


postulate
   NumAndStringList : Set
   --
   getNum : NumAndStringList → ℕ
   getStringList : NumAndStringList → List String
   --
   getEventsFFIhs : SDLWindow → FFIComponents → IO NumAndStringList


{-# COMPILE GHC NumAndStringList       = type M.NumAndStringList #-}
{-# COMPILE GHC getNum                 = M.getNum #-}
{-# COMPILE GHC getStringList          = M.getStringList #-}
{-# COMPILE GHC getEventsFFIhs         = M.getEventsFFI #-}



getEventsFFI : SDLWindow → Frame → IO (ℕ × (List String))
getEventsFFI win fr = do
  x <- getEventsFFIhs win (frame2FFI fr)
  return ( getNum x , getStringList x)


--
-- Functions
--
postulate
  createWindowFFI : IO SDLWindow
  renderFrameToScreenFFIhs : SDLWindow → FFIComponents → IO Unit

{-# COMPILE GHC createWindowFFI      = SDL.createWindow #-}
{-# COMPILE GHC renderFrameToScreenFFIhs  = R.renderFrameToScreen #-}


renderFrameToScreenFFI : SDLWindow → Frame → IO Unit
renderFrameToScreenFFI win fr = renderFrameToScreenFFIhs win (frame2FFI fr)


{-
  getButtonClickedFFIhs : FFIComponents → IO (Maybe  ℕ)
  --

  --
  showMaybe : Maybe  ℕ → IO Unit

-- OLD: delete this: getButtonClicked : IO (Maybe ℕ)





{-# COMPILE GHC getButtonClickedFFIhs  = M.getButtonClickedFromFrame #-}
{-# COMPILE GHC showMaybe  = (\x -> putStrLn $ show x) #-}


--
-- TODO: remember to only count the number of buttons
--       If I had used the 'length' of the frame list in past
--       then this length would be now wrong (!!)
--


getButtonClickedFFI : Frame → IO (Maybe  ℕ)
getButtonClickedFFI fr = getButtonClickedFFIhs (frame2FFI fr)

-- List string is the values of the Textboxes
--
postulate getEventsFFIOld : Frame → IO (ℕ × (List String))
-}







