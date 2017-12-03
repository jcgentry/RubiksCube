module Applicable where

import Cube

class Applicable a where
  apply :: a -> Cube -> Cube