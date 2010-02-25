module Main where

import qualified Data.ByteString as BS
import Data.Char
import Data.List
import System.Directory

main :: IO ()
main = do
  a <- getDirectoryContents "Language/PowerPC/Instructions"
  let modules = [ take (length i - 3) i | i <- a, isSuffixOf ".hs" i ]
      names   = map (map toLower) modules
  writeFile "Language/PowerPC/Instructions.hs" $ unlines
    [ "-- | A list of all instructions.  File generated by SetupInstrs.hs"
    , "module Language.PowerPC.Instructions (instructions) where"
    , ""
    , "import Language.PowerPC.Instruction"
    , unlines [ "import Language.PowerPC.Instructions." ++ m | m <- modules ]
    , "-- | All available instructions."
    , "instructions :: [Instruction]"
    , "instructions ="
    , "  [ " ++ intercalate "\n  , " names
    , "  ]"
    ]

