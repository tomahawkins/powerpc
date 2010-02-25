module Main where

import qualified Data.ByteString as BS

import Language.PowerPC

main :: IO ()
main = BS.readFile "test" >>= simulate 0 0x100

