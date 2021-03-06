module GUI.BusinessProcessMedExVers4Compiled  where


open import GUI.BusinessProcessMedExVers4
open import GUI.RasterificFFI
open import GUI.GUICompilation hiding (main)

open import NativeIO renaming (NativeIO to IO;
                               nativeReturn to return;
                               _native>>=_ to _>>=_;
                               _native>>_ to _>>_)

main : IO Unit
main = do
  win <- createWindowFFI
  compile win patientRegistrationGUI
