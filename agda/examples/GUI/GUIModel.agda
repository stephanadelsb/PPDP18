--@PREFIX@GUIModel

module GUI.GUIModel where

-- open import GUIgeneric.Prelude renaming (inj₁ to secondButton; inj₂ to firstButton)

-- open import GUIgeneric.PreludeGUI renaming (WxColour to Color) hiding (IIOnterfaceˢ)

-- open import GUIgeneric.GUIDefinitions renaming (add to add'; add' to add) --; ComponentEls to Frame)
-- open import GUIgeneric.GUI
-- open import GUIgeneric.GUIExampleLib
-- open import GUIgeneric.GUIExample -- hiding (HandlerGUIObject)

open import StateSizedIO.GUI.BaseStateDependent
open import StateSizedIO.writingOOsUsingIOVers4ReaderMethods hiding (nˢ) renaming(execˢⁱ to execᵢ ; returnˢⁱ to returnᵢ)
open import GUI.GUIDefinitions
open import GUI.GUIDefinitionsBase
open import SizedIO.Base  renaming (IO to IO∞; IO' to IO)
open import SizedIO.Console

-- open IOInterfaceˢ public

open import Data.Product
open import Data.Nat
open import Data.Fin
open import Data.String
open import Data.Unit
open import Data.Maybe
open import Size
open import Relation.Binary.PropositionalEquality.Core
open import Relation.Binary.PropositionalEquality hiding (setoid ; preorder ; decSetoid; [_])
open import Data.Sum hiding (map)

-- infix 3 _goesThru_

infixl 3 _>>>_

-- How many trivial io commands such as putStrLn are ignored in the model
skippedIOcmds : ℕ
skippedIOcmds = 2

IOCommand = ConsoleCommand
IOResponse = ConsoleResponse

-- test = _×_

-- cmpStruc : CompStruc
-- cmpStruc = compStrucEx

-- c : Components compStrucEx
-- c = frame


-- StateAndGuiObj : Set
-- StateAndGuiObj = Σ[ s ∈ Frame ] (FrameObj {∞} s )

-- was Σ[ s ∈ GUIStateˢ compStrucEx frame ] (GUIel compStrucEx c) × (FrameGUIObj {∞} s )

-- --@BEGIN@MethodStarted

-- data MethodStarted (s : GUIStateˢ compStrucEx c)
--                    (obj : FrameGUIObj {∞} s) : Set where
--    notStarted : MethodStarted s obj
--    started :    (m    : GUIMethodˢ compStrucEx frame s) (pr : IO consoleI ∞ StateAndGuiObj)
--                 → MethodStarted s obj

-- --@END

--@BEGIN@MethodStarted
data MethodStarted (g : GUI) : Set where
   notStarted : MethodStarted g
   started :   (m    : GUIMethod g)
               (pr : IO consoleI ∞ GUI)
               → MethodStarted g
--@END

--@BEGIN@ModelGuiState
data GuiState : Set where
   state : (g       : GUI)
           (m       : MethodStarted g) → GuiState
--@END



--@BEGIN@modelGuiCommand
GuiCmd : (s : GuiState) → Set
GuiCmd  (state g notStarted)
       = GUIMethod g
GuiCmd  (state g (started m (exec' c f)))
       = IOResponse c
GuiCmd  (state g (started m (return' a)))
       = ⊤
--@END

-- modelGuiResponse : Set
--@BEGIN@modelGuiResponse
-- modelGuiResponse = ⊤
--@END





handlerReturnTypeToGUI :
          (g       : GUI)
          (m       : GUIMethod  g)
          (res :  Σ[ r ∈ GUIResult g m ]
                  FrameObj {∞} (nextGUIFrame g m r))
           → GUI
handlerReturnTypeToGUI g m (r , obj') = guic (nextGUIFrame g m r)  obj'

--  handlerReturnTypeToGUI g prop obj (changedAttributes prop' , obj') = g , prop' , obj'
--   handlerReturnTypeToGUI g prop obj (changedGUI g' prop' , obj') = g' , prop' , obj'

-- Old name in old code was: "HandlerIOType"
--

GUIObjIOType : (i : Size)(s : Frame)
               (m : FrameMethod  s)
               → Set
GUIObjIOType i s m = IO∞ consoleI ∞
                        (Σ[ r ∈ FrameResult s m ]
                           FrameObj  {∞} (nextFrame s m r))


guiNextProgramStarted : (g : GUI)
                              (m : GUIMethod g)
                              → IO consoleI ∞ GUI
guiNextProgramStarted g m =
     force (fmap ∞  (handlerReturnTypeToGUI g m) (g .obj .method m))



guiNextaux : (s : GUI)
                  (m : GUIMethod s)
                  (pr : IO consoleI ∞ GUI)
                  (skippedCms : ℕ)
                  → GuiState
guiNextaux g m (exec' (putStrLn s₁) f) (suc n) =
    guiNextaux g m (force (f _)) n
guiNextaux g m  (exec' c' f) n =
        state g (started m (exec' c' f))
guiNextaux g m  (return' (guic sNew  objNew)) n =
        state (guic sNew objNew) notStarted



--@BEGIN@modelGuiNext
guiNext : (g : GuiState) → GuiCmd g →  GuiState
--@END
guiNext (state g notStarted) m     =
       guiNextaux g m  (guiNextProgramStarted g m) skippedIOcmds
guiNext (state g (started m (exec' c' f))) c =
       guiNextaux g m (force (f c)) skippedIOcmds
guiNext (state g (started m (return' (guic sNew  objNew)))) c =
         state (guic sNew objNew) notStarted

mutual
--\GUIModel
--@BEGIN@ModelGuiCommands
   data GuiCmds : GuiState → Set where
     nilCmd : {g : GuiState} →  GuiCmds g
     _>>>_ :  {g : GuiState} (l : GuiCmds g)
              (c : GuiCmd (guiNexts g l))
              → GuiCmds g

   guiNexts : (g : GuiState) → GuiCmds g → GuiState
--@END
   guiNexts g nilCmd = g
   guiNexts g (l >>> c') = guiNext (guiNexts g l) c'

--@BEGIN@arrowGui
data _-gui->_ (s : GuiState) :
              (s' : GuiState ) → Set where
 refl-gui-> :  s -gui-> s
 step       :  {s' : GuiState}(c : GuiCmd s)
               (next : guiNext s c -gui-> s')
               → s -gui-> s'
--@END


--@BEGIN@arrowGuiOne
data _-gui->¹_ (s : GuiState )
               : (s' : GuiState)→ Set where
   step :  (c : GuiCmd s)
           → s -gui->¹ guiNext s c
--@END






nextGui : (s : GuiState)(c : GuiCmd s) → GuiState
nextGui s c = guiNext s c



modelToIOprog : (g : GuiState) → Maybe (IO consoleI ∞ GUI)
modelToIOprog (state g notStarted) = nothing
modelToIOprog (state g (started s₁ pr)) = just pr


nextGuiProg : (s : GuiState)(c : GuiCmd s)
              → Maybe (IO consoleI ∞ GUI)
nextGuiProg s c = modelToIOprog (nextGui s c)



guiNext' : (s : GuiState)(c : GuiCmd s)
               → GuiState
guiNext' (state g notStarted) m     =
       state g (started m (guiNextProgramStarted g m))
guiNext' (state g (started m (exec' c' f))) c =
       state g (started m (force (f c)))
guiNext' (state g (started m (return' (guic sNew  objNew)))) c =
       state (guic sNew objNew) notStarted



data _-gui->¹'_ (m : GuiState ) : (m' : GuiState)→ Set where
   step : (c : GuiCmd m) → m -gui->¹' guiNext' m c

nextGui' : (m : GuiState)(c : GuiCmd m) → GuiState
nextGui' m c = guiNext' m c



--
-- This is from GUIModelAdvancedsimplified in old code:
--
--@BEGIN@GoesThruState

_goesThru_ :  {s s' : GuiState}
              (q : s -gui-> s')
              (t : GuiState) → Set
_goesThru_ {s} (step c q) t   =  s ≡ t ⊎ q goesThru t
_goesThru_ {s} refl-gui-> t   =  s ≡ t

--@END
--@BEGIN@GoesThruSelf


-- _goesThru_ {s} (execᵢ c f) t = s ≡ t ⊎ (f _) goesThru t
-- _goesThru_ {s} (returnᵢ a) t = s ≡ t


goesThruSelf : {s s' : GuiState} (q : s -gui-> s')
               → q goesThru s

--@END
goesThruSelf (step c next) = inj₁ refl
goesThruSelf refl-gui->    = refl

-- goesThruSelf (execᵢ c f) = inj₁ refl
-- goesThruSelf (returnᵢ a) = refl

--\GUIModel
--@BEGIN@guiEventuallyState
data  _-eventually->_ :
      (start final : GuiState) → Set where
  hasReached  :  {s : GuiState} → s -eventually-> s
  next :  {start final : GuiState}
          (fornext :  (m :  GuiCmd start)
                      →  (guiNext start m) -eventually-> final)
          → start -eventually-> final
--@END

-- We define short forms for inputs used in the model
--    used as GuiCmd
--    in  guiNext  and guiNexts

btnClick : Fin 1 × ⊤
btnClick = (zero , _)

textboxInput : String → Fin 1 × String
textboxInput str = (zero , str)

textboxInput2 : String → String → Fin 1 × String × String
textboxInput2 str str' = (zero , str , str')
