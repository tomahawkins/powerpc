module Language.PowerPC.Utils
  ( debug
  , debug'
  ) where

import System.IO.Unsafe

debug :: Show a => String -> a -> a
debug m a = debug' m a a

debug' :: Show a => String -> a -> b -> b
debug' m a b = unsafePerformIO (putStrLn (m ++ ": " ++ show a) >> return b)

