module Trial(Trial(..), doTrial) where

import Cube
import Pattern
import Turn

limit = 5000

data Trial = Trial {
  start :: Cube,
  turns :: [Turn],

  -- The pattern we tried to apply--note that in some cases it might be solved before completion
  attemptedPattern :: Pattern,
  repeated :: Bool,

  isSolved :: Bool,

  endingCube :: Cube
}

initTrial start pattern repeated = Trial {
  start = start,
  turns = [],
  attemptedPattern = pattern,
  repeated = repeated,
  isSolved = False,
  endingCube = start
}

doTrial :: Cube -> Pattern -> Bool -> Trial
doTrial start pattern@(Pattern turns) repeat
  | solved start =
      Trial {
        start = start,
        turns = [],
        attemptedPattern = pattern,
        repeated = repeat,
        isSolved = True,
        endingCube = start
      }
  | repeat = trial start (cycle turns) (initTrial start pattern True)
  | otherwise = trial start turns (initTrial start pattern False)


trial :: Cube -> [Turn] -> Trial -> Trial

trial cube [] trial = trial {
  isSolved = False,
  endingCube = cube
}

trial cube (t:ts) currentTrial =
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
            currentTrial {
              turns = (turns currentTrial) ++ [t],
              endingCube = cube'
            }
