module Print where

class (Show a) => Print a where

  dump :: a -> IO ()
  dump = putStr . show

  dumpln :: a -> IO ()
  dumpln x = do
                dump x
                putStrLn ""

instance Print x => Print [x] where
  dump (c:cs) = do
                  dump c
                  dump cs
  dump [] = return ()