module Trial(Trial(..), doTrial, showTrial) where

import Cube
import Pattern
import Turn

defaultLimit = 5000

data Trial = Trial {
  start :: Cube,
  turns :: [Turn],

  -- The pattern we tried to apply--note that in some cases it might be solved before completion
  attemptedPattern :: Pattern,
  repeated :: Bool,

  isSolved :: Bool,

  limit :: Int,

  endingCube :: Cube
}

instance Show Trial where
  show (Trial _ turns attemptedPattern repeated isSolved _ _) =
    "turns = " ++ (show turns) ++ ", isSolved = " ++ (show isSolved)

initTrial start pattern repeated limit = Trial {
  start = start,
  turns = [],
  attemptedPattern = pattern,
  repeated = repeated,
  isSolved = False,
  limit = limit,
  endingCube = start
}

doTrial :: Cube -> Pattern -> Bool -> Int -> Trial
doTrial start pattern@(Pattern turns) repeat limit
  | solved start =
      Trial {
        start = start,
        turns = [],
        attemptedPattern = pattern,
        repeated = repeat,
        isSolved = True,
        limit = limit,
        endingCube = start
      }
  | repeat = trial start (cycle turns) limit (initTrial start pattern True limit)
  | otherwise = trial start turns limit (initTrial start pattern False limit)


trial :: Cube -> [Turn] -> Int -> Trial -> Trial

trial cube [] _ trial = trial {
  isSolved = False,
  endingCube = cube
}

trial _ _ 0 trial = trial {
  isSolved = False
}

trial cube (t:ts) limit currentTrial =
  let cube' = applyTurn t cube in
    if (solved cube')
      then currentTrial {
        turns = (turns currentTrial) ++ [t],
        isSolved = True,
        endingCube = cube'
      }
     else trial
            cube'
            ts
            (limit - 1)
            currentTrial {
              turns = (turns currentTrial) ++ [t],
              endingCube = cube'
            }

showTrial :: Cube -> Pattern -> Bool -> IO Trial
showTrial cube pattern repeat =
  let result = doTrial cube pattern repeat defaultLimit
    in do
        return result