module Patterns where

import Pattern
import Turn

checkerboard :: Pattern
checkerboard = Pattern [L2, R2, U2, D2, F2, B2]

-- Fake pattern for allegedly solving any cube - https://www.youtube.com/watch?v=D3g1BH6Yo8M&t=160s
wilson :: Pattern
wilson = Pattern [L, R2, B, L', R, U2, F2, L, B, D, U2, B, L', R2, B2, L', D2, U, L', B2, U2, R2, B, D2]