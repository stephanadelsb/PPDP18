module GUI.LoadAllPPDP18 where

-- this file loads the files containing the code examples in the
-- PPDP18 paper of Andreas Adelsberger, Anton Setzer and Eric Walkinshaw, ordered by sections

-- Abstract
-- 1. Introduction
-- 2. Background

open import SizedIO.Base

-- 3. A Library for Declarative, State-Dependent GUI programming
-- 3.1. Introductory Example: A GUI with Infinitely Many States

open import GUI.GUIExampleInfiniteBtnsAdvanced

-- 3.2. State-dependent Objects

open import StateSizedIO.GUI.BaseStateDependentWithoutSizes

-- 3.3. A Data Type for GUIs

open import GUI.GUIDefinitionsSpecific
open import GUI.GUIExampleLib

-- 3.4. Implementation Details

open import GUI.GUICompilation
open import GUI.RasterificFFI

-- The Haskell code for the FFI interface can be found in
-- examples/GUI/DeclareGUI/*.hs

-- 4. Reasoning About GUI Applications

-- 4.1. Reasoning about Coinductive Programs

open import GUI.GUIModelRenamed

-- 4.2 Properties Over GUI Application States

open import GUI.GUIModelRenamed

-- 5. Case Study: Healthcare Process Models
-- 5.1. A Process for Prescribing Oral Anticoagulants
-- 5.2. Formalization of Business Processes

open import GUI.BusinessProcess
open import GUI.BusinessProcessMedExLib
open import GUI.BusinessProcessMedExVers3

-- Advanced example with check conditions for inputs mentioned in footnote 6

open import GUI.BusinessProcessVers2
open import GUI.BusinessProcessMedExVers4

-- 5.3. From Business Processes to GUI Applications

open import GUI.BusinessProcessMedExVers3
open import GUI.BusinessProcessMedExVers3Compiled

-- Advanced example with check conditions for inputs mentioned in footnote 6

open import GUI.BusinessProcessMedExVers4
open import GUI.BusinessProcessMedExVers4Compiled

-- 5.4. Verifying GUI Applications

open import GUI.BusinessProcessMedExVers3
open import GUI.BusinessProcessMedExVers3Part2

-- Advanced example with check conditions for inputs mentioned in footnote 6

open import GUI.BusinessProcessMedExVers4
open import GUI.BusinessProcessMedExVers4Part2
