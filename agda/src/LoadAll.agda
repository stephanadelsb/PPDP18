


module LoadAll where



-- 1)) NativeIO etc.
--
open import Unit -- DEPRECATE THIS
open import NativeString
open import NativeInt

open import NativeIO
open import ConsoleLib


-- 2a)) STATEFUL (rename to StateIO)  -- @@474 occurences
--
open import StateSizedIO.IOObject
open import StateSizedIO.writingOOsUsingIO
--open import StateSizedIO.writingOOsUsingIOVers3ReaderMethods   -- needs fixing additional levels!!!
--open import StateSizedIO.writingOOsUsingIOVers2
--open import StateSizedIO.writingOOsUsingIOVers3

open import StateSizedIO.BaseNonPoly
open import StateSizedIO.Object
open import StateSizedIO.RObject
open import StateSizedIO.cellStateDependent

-- 2b)) GUI
--
open import StateSizedIO.GUI.VariableList
open import StateSizedIO.GUI.WxGraphicsLibLevel3
open import StateSizedIO.GUI.WxGraphicsLib
open import StateSizedIO.GUI.WxGraphicsLibLevel3Generic
open import StateSizedIO.GUI.WxGraphicsLibLevel3WithDB
open import StateSizedIO.GUI.WxGraphicsLibLevel3WithDBPrime
open import StateSizedIO.GUI.WxBindingsFFI
open import StateSizedIO.GUI.Test
open import StateSizedIO.GUI.Prelude

--
-- WHY IS THIS FILE NOT ONE LEVEL HIGHER??? : 
--
open import StateSizedIO.GUI.BaseStateDependent 
open import StateSizedIO.GUI.MainDB  --[[STEPHAN]]


-- 3)) SizedIO (rename to IO)
--
-- SizedIO  [renamed to IO]   @@615 occurences
--
open import SizedIO.IOObject
open import SizedIO.Console
open import SizedIO.ConsoleObject
open import SizedIO.writingOOUsingIO
open import SizedIO.Object

open import SizedIO.Base
open import SizedIO.IOGraphicsLib -- DEPRECATE THIS, still uses SEO library



-- 4)) coIOIO
--
open import coIOIO.objectCoObject
open import coIOIO.objectCoObjectVers2
-- not working:
-- open import coIOIO.objectCoObjectIOVers2   [[ANTON]]

open import coIOIO.objectCoObjectVers2Defs
open import coIOIO.objectCoObjectIOVers2Defs


-- 5)) heap
--
open import heap.libraryMaybe

--
-- 6))) Start deprecated stuff:
--
-- (also move all old stuff into toplevel old directory)
--



{-
-- 6a))) Deprecated?? PolyNativeIO   @@60 occurences
--
open import NativePolyIO   -- Deprecate PolyIO ??
open import SizedPolyIO.IOObject
open import SizedPolyIO.Console
open import SizedPolyIO.ConsoleObject
open import SizedPolyIO.Object
open import SizedPolyIO.Base
-}


-- 6b)) Deprecated: Unsized
--
open import UnSizedIO.IOObject
open import UnSizedIO.Console
open import UnSizedIO.ConsoleObject
open import UnSizedIO.Object
open import UnSizedIO.Base 


-- ADD 1)) Misc heap stuff (that I don't know how to fix)
--
-- Heap and related things
--
{-
HEAP CHANGES:

*) IMPORT open import StateSizedIO.GUI.BaseStateDependent
*) RENAME
   IOnextˢ ==> nextˢ
   IOStateˢ == Stateˢ
*) fmapˢ and fmapˢ' have size and state implicit!
-}


-- NOT easy to fix: open import heap.heapAsObjectAttempt6NativeHeap

open import heap.libraryProduct
open import heap.libraryNat

-- NOT clear hwo to fix; done by Andreas:
-- open import heap.SeparationLogic

-- Unsolved Metas:
-- open import heap.heapAsObjectAttempt7

open import heap.library
open import heap.heapAsObjectAttempt2
open import heap.heapAsObjectAttempt7NativeHeap

open import heap.heapAsObject

open import heap.heapAsObjectGeneric
-- open import heap.Unused -- DEPRECATE THIS!!!
open import heap.libraryList
open import heap.haskellNativeHeap
{-
open import heap.heapAsObjectAttempt4String

-- open import heap.heapAsObjectAttempt5String

open import heap.heapAsObjectAttempt3String

open import heap.old.heapBasedOnNat2
open import heap.libraryFin
open import heap.heapAsObjectAttempt3
open import heap.heapAsObjectAttempt4
open import heap.heapBasedOnNat
open import heap.worldModuleGeneric
open import heap.worldModule
open import heap.libraryBool
open import heap.heapOverGeneralisedWorlds
open import heap.heapAsObjectAttempt6String
open import heap.heapBasedOnNatAttempt2
open import heap.heapAsObjectAttempt6BinaryHeap
open import heap.libraryEq
-}



-- Misc Library stuff (need to look into how to fix this)
--

open import StateSizedIO.LIB.DB.Conversion   -- Delete/deprecate this; doesn't seem to be used!
open import StateSizedIO.LIB.DB.TestNodeTable
open import StateSizedIO.LIB.DB.Database
open import StateSizedIO.LIB.DB.Serialization
-- open import StateSizedIO.LIB.DB.TestMain -- needs fixing?
open import StateSizedIO.LIB.DB.TestSchemaEdges  
open import StateSizedIO.LIB.DB.Dictionary
open import StateSizedIO.LIB.DB.Interface
open import StateSizedIO.LIB.DB.Schema
open import StateSizedIO.LIB.DB.Query
open import StateSizedIO.LIB.DB.DBUniverse
open import StateSizedIO.LIB.Threepenny.Threepenny
open import StateSizedIO.LIB.HDBC.library
open import StateSizedIO.LIB.HDBC.Unused
open import StateSizedIO.LIB.HDBC.HDBC








