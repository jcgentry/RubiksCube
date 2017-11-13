module Color(Color(..), ansiColor) where

import Print
import qualified System.Console.ANSI as ANSI

square = "*"

data Color = Red | Orange | Yellow | Green | Blue | White deriving (Eq, Show)

instance Print Color where
  print color = do
                   ANSI.setSGR [ANSI.SetColor ANSI.Foreground ANSI.Vivid (ansiColor color)]
                   putStr square
                   putStr " "
                   ANSI.setSGR [ANSI.Reset]

ansiColor :: Color -> ANSI.Color
ansiColor Red = ANSI.Red
ansiColor Orange = ANSI.Magenta
ansiColor Yellow = ANSI.Yellow
ansiColor Green = ANSI.Green
ansiColor Blue = ANSI.Blue
ansiColor White = ANSI.Black