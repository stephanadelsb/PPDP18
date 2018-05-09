
module GUI.GUIModelRenamed where

-- open import GUIgeneric.Prelude renaming (inj₁ to secondButton; inj₂ to firstButton)

-- open import GUIgeneric.PreludeGUI renaming (WxColor to Color) hiding (IIOnterfaceˢ)

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
open import Relation.Binary.PropositionalEquality.Core public
open import Relation.Binary.PropositionalEquality hiding (setoid ; preorder ; decSetoid; [_]) public
open import Data.Sum public hiding (map)

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

-- 

-- data MethodStarted (s : GUIStateˢ compStrucEx c)
--                    (obj : FrameGUIObj {∞} s) : Set where
--    notStarted : MethodStarted s obj
--    started :    (m    : GUIMethodˢ compStrucEx frame s) (pr : IO consoleI ∞ StateAndGuiObj)
--                 → MethodStarted s obj

-- 

data MethodStarted (g : GUI) : Set where
   notStarted : MethodStarted g
   started :   (m    : GUIMethod g)
               (pr : IO consoleI ∞ GUI) → MethodStarted g

data State : Set where
   state : (g : GUI) → MethodStarted g → State




Cmd :  State → Set
Cmd  (state g notStarted)               =  GUIMethod g
Cmd  (state g (started m (exec' c f)))  =  IOResponse c
Cmd  (state g (started m (return' a)))  =  ⊤

-- modelGuiResponse : Set
-- modelGuiResponse = ⊤





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
                  → State
guiNextaux g m (exec' (putStrLn s₁) f) (suc n) =
    guiNextaux g m (force (f _)) n
guiNextaux g m  (exec' c' f) n =
        state g (started m (exec' c' f))
guiNextaux g m  (return' (guic sNew  objNew)) n =
        state (guic sNew objNew) notStarted



guiNext : (g : State) → Cmd g →  State
guiNext (state g notStarted) m     =
       guiNextaux g m  (guiNextProgramStarted g m) skippedIOcmds
guiNext (state g (started m (exec' c' f))) c =
       guiNextaux g m (force (f c)) skippedIOcmds
guiNext (state g (started m (return' (guic sNew  objNew)))) c =
         state (guic sNew objNew) notStarted

mutual
--\GUIModel
   data Cmds : State → Set where
     nilCmd : {g : State} →  Cmds g
     _>>>_ :  {g : State} (l : Cmds g)→ Cmd (guiNexts g l)
              → Cmds g

   guiNexts : (g : State) → Cmds g → State
   guiNexts g nilCmd = g
   guiNexts g (l >>> c') = guiNext (guiNexts g l) c'

data _-gui->_ (s : State) :  State → Set where
 refl-gui-> :  s -gui-> s
 step       :  {s' : State}(c : Cmd s)
               →  guiNext s c -gui-> s'
               →  s -gui-> s'


data _-gui->¹_ (s : State )
               : (s' : State)→ Set where
   step :  (c : Cmd s) → s -gui->¹ guiNext s c






nextGui : (s : State)(c : Cmd s) → State
nextGui s c = guiNext s c



modelToIOprog : (g : State) → Maybe (IO consoleI ∞ GUI)
modelToIOprog (state g notStarted) = nothing
modelToIOprog (state g (started s₁ pr)) = just pr


nextGuiProg : (s : State)(c : Cmd s)
              → Maybe (IO consoleI ∞ GUI)
nextGuiProg s c = modelToIOprog (nextGui s c)



guiNext' : (s : State)(c : Cmd s)
               → State
guiNext' (state g notStarted) m     =
       state g (started m (guiNextProgramStarted g m))
guiNext' (state g (started m (exec' c' f))) c =
       state g (started m (force (f c)))
guiNext' (state g (started m (return' (guic sNew  objNew)))) c =
       state (guic sNew objNew) notStarted



data _-gui->¹'_ (m : State ) : (m' : State)→ Set where
   step : (c : Cmd m) → m -gui->¹' guiNext' m c

nextGui' : (m : State)(c : Cmd m) → State
nextGui' m c = guiNext' m c



--
-- This is from GUIModelAdvancedsimplified in old code:
--

_goesThru_ :  {s s' : State}
              (q : s -gui-> s')
              (t : State) → Set
_goesThru_ {s} (step c q) t   =  s ≡ t ⊎ q goesThru t
_goesThru_ {s} refl-gui-> t   =  s ≡ t



-- _goesThru_ {s} (execᵢ c f) t = s ≡ t ⊎ (f _) goesThru t
-- _goesThru_ {s} (returnᵢ a) t = s ≡ t


goesThruSelf : {s s' : State} (q : s -gui-> s')
               → q goesThru s

goesThruSelf (step c next) = inj₁ refl
goesThruSelf refl-gui->    = refl

-- goesThruSelf (execᵢ c f) = inj₁ refl
-- goesThruSelf (returnᵢ a) = refl

--\GUIModel
data  _-eventually->_ :
      (start final : State) → Set where
  hasReached  :  {s : State} → s -eventually-> s
  next :  {start final : State}
          (fornext : (m :  Cmd start)
                     →  (guiNext start m) -eventually-> final)
          →  start -eventually-> final

-- We define short forms for inputs used in the model
--    used as Cmd
--    in  guiNext  and guiNexts

btnClick : Fin 1 × ⊤
btnClick = (zero , _)

textboxInput : String → Fin 1 × String
textboxInput str = (zero , str)

textboxInput2 : String → String → Fin 1 × String × String
textboxInput2 str str' = (zero , str , str')
