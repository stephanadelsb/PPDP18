--{-# OPTIONS --allow-unsolved-metas #-}

module StateSizedIO.GUI.Test where


open import StateSizedIO.GUI.Prelude
open import StateSizedIO.GUI.WxGraphicsLibLevel3 hiding (main)


main' : IO GuiInterface ∞ Unit
main' =
  exec  createFrame                 λ frame  →
  exec  (createButton frame "btn 1")  λ btn1 →
  exec  (createButton frame "btn 2")  λ btn2 →
  exec  (createTextCtrl frame "hello!!")  λ txtCtrl →
  exec  (setChildredLayout frame 3 10 2 2) λ yy →
  exec  (putStrLn "Frame created")  λ _      →
  return _

main : NativeIO Unit
main = start (translateIO translateLev1Local main')
