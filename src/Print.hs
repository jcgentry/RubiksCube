module Print where

class (Show a) => Print a where

  print :: a -> IO ()
  print = putStr . show

  println :: a -> IO ()
  println x = do
                Print.print x
                putStrLn ""

instance Print x => Print [x] where
  print (c:cs) = do
                  Print.print c
                  Print.print cs
  print [] = return ()