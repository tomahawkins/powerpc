module Main where

import qualified Data.ByteString as BS
import Data.IORef
import qualified Data.Map as M
import Data.Word
import Text.Printf

import Language.PowerPC

main :: IO ()
main = do
  p <- BS.readFile "test"
  m <- memory p
  simulate 0 0x100 p m

memory :: BS.ByteString -> IO Memory
memory p = do
  ram <- newIORef M.empty
  return Memory { load = load ram, store = store ram }
  where
  load :: IORef (M.Map Word64 Word8) -> Word64 -> Int -> IO [Word8]
  load ram addr bytes
    | fromIntegral addr + bytes - 1 < BS.length p = return $ map (BS.index p . fromIntegral) addrs
    | addr >= 0x3F8000 && addr < 0x400000 = do
      ram <- readIORef ram
      return $ map (ram M.!) addrs
    | otherwise = printf "load  0x%08X %d bytes\n" addr bytes
    where
    addrs = [addr .. addr + fromIntegral bytes - 1]

  store :: IORef (M.Map Word64 Word8) -> Word64 -> [Word8] -> IO ()
  store ram addr bytes
    | addr >= 0x3F8000 && addr < 0x400000 = do
      r <- readIORef ram
      writeIORef ram $ foldl (\ m (addr, byte) -> M.insert addr byte m) r $ zip [addr ..] bytes
    | otherwise = printf "store 0x%08X 0x" addr >> mapM_ (printf "%02X") bytes >> printf "\n"
  
