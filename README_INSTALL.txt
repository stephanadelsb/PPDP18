

## 1) Agda installation
(we are using '2.6.0-8cae647' but later versions should work)

## 2)  Agda standard lib installation
(we are using version 55ad461aa4fc6cf22e97812b7ff8128b3c7a902c)

## 3) Install hackage 'sdl2' library
##
## For Ubuntu do this:
## (for other linux version, exchange the "apt-get" command)
sudo apt-get install libsdl2-dev
cabal update
cabal install sdl2

## 4) Install hackage version of Rasterific
##
cabal update
cabal install rasterific
cabal install rasterific-svg

## 5) In emacs open the file:
##   './PPDP18/agda/examples/GUIBusinessProcessMedExVers4Compiled.agda'
##   and use 'Agda->Compile' in emacs, 
##   then choose 'GHC' as the backend.



Small tip: Sometimes it helps removing the 'MAlonzo' directory and recompilling everything.
