module Pretty(Printable, PrettyElement(..), pretty, prettyPrint) where

import Color

import qualified System.Console.ANSI as ANSI

data PrettyElement = ColorText Color String | Spaces Int | Text String | NewLine

class Printable x where
  pretty :: x -> [PrettyElement]

printElement :: PrettyElement -> IO ()
printElement (ColorText c s) = do
                                ANSI.setSGR [ANSI.SetColor ANSI.Foreground ANSI.Vivid (ansiColor c)]
                                ANSI.setSGR [ANSI.SetColor ANSI.Background ANSI.Dull ANSI.Black]
                                putStr s
printElement (Text s) = do
                                ANSI.setSGR [ANSI.Reset]
                                putStr s
printElement (Spaces x) = do
                                ANSI.setSGR [ANSI.SetColor ANSI.Background ANSI.Dull ANSI.Black]
                                putStr $ replicate x ' '
printElement NewLine = printLineReturn

printLineReturn :: IO ()
printLineReturn = do
                      ANSI.setSGR [ANSI.Reset]
                      putStrLn ""

printElements :: [PrettyElement] -> IO ()
printElements [] = do
                    ANSI.setSGR [ANSI.Reset]
                    printLineReturn
printElements (e:es) = do
                        printElement e
                        printElements es


ansiColor :: Color -> ANSI.Color
ansiColor Red = ANSI.Red
ansiColor Orange = ANSI.Magenta   -- Alas, ANSI doesn't seem to have an "orange"
ansiColor Yellow = ANSI.Yellow
ansiColor Green = ANSI.Green
ansiColor Blue = ANSI.Blue
ansiColor White = ANSI.White

prettyPrint :: Printable p => p -> IO ()
prettyPrint p = printElements $ pretty p

