


module DeclareGUI.ResolveConstraints where


import  DeclareGUI.Definitions

--
-- !! Dangerous: We assume that Textboxes and Buttons are never overlapping
--   
--

resolveConstraintsOfWindow :: Window -> Time -> IO (Maybe ConstraintResult)
resolveConstraintsOfWindow win@(Window _ constraints _ ) time =
  resolveConstraints constraints win time

--
-- !! Dangerous: Constraints are not recursive yet
--  if one constraint triggers another, this is currently ignored
--

resolveConstraints :: [Constraint] -> Window -> Time -> IO (Maybe ConstraintResult)
resolveConstraints (constraint@(When pred num) : cs) win time
  | (pred win time) = return $ Just num
  | otherwise = resolveConstraints cs win time
resolveConstraints [] win time = return Nothing

