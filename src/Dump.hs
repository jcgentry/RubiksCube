module Dump where

class Dump a where

  dump :: a -> IO ()

  dumpln :: a -> IO ()
  dumpln x = do
                dump x
                putStrLn ""

instance Dump x => Dump [x] where
  dump (c:cs) = do
                  dump c
                  dump cs
  dump [] = return ()