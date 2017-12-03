module Dump where

class (Show a) => Dump a where

  dump :: a -> IO ()
  dump = putStr . show

  dumpln :: a -> IO ()
  dumpln x = do
                dump x
                putStrLn ""

instance Dump x => Dump [x] where
  dump (c:cs) = do
                  dump c
                  dump cs
  dump [] = return ()