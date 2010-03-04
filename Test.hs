#!/home/e0082888/.ghc/bin/runhaskell -W
module Main where

import Data.Bits
import qualified Data.ByteString as BS
import Data.IORef
import qualified Data.Map as M
import Data.Word
import System.IO
import Text.Printf

import Language.PowerPC

main :: IO ()
main = do
  p <- BS.readFile "test"
  m <- memory p
  simulate m 0x100

data Mem = Mem BS.ByteString (IORef (M.Map Word64 Word8))

memory :: BS.ByteString -> IO Mem
memory p = do
  ram <- newIORef M.empty
  return $ Mem p ram

instance Memory Mem where
  load mem addr bytes = do
    (section, bytes) <- load' mem addr bytes
    printf "load  (%s) 0x%08X 0x" section addr >> mapM_ (printf "%02X") bytes >> printf "\n"
    hFlush stdout
    return bytes

  store mem addr bytes = do
    section <- store' mem addr bytes
    printf "store (%s) 0x%08X 0x" section addr >> mapM_ (printf "%02X") bytes >> printf "\n"
    hFlush stdout

  fetch (Mem p _) addr
    | fromIntegral addr + 4 - 1 < BS.length p = return $ shiftL (fromIntegral $ BS.index p $ fromIntegral $ addr + 0) 24 
                                                     .|. shiftL (fromIntegral $ BS.index p $ fromIntegral $ addr + 1) 16
                                                     .|. shiftL (fromIntegral $ BS.index p $ fromIntegral $ addr + 2)  8
                                                     .|. shiftL (fromIntegral $ BS.index p $ fromIntegral $ addr + 3)  0
    | otherwise = error $ printf "Invalid program address: 0x%08X" addr

load' (Mem p ram) addr bytes
  | fromIntegral addr + bytes - 1 < BS.length p = return $ ("bootloader", map (BS.index p . fromIntegral) addrs)
  | inSpace addr 0x3F8000 0x8000 || inRange addr 0x2F8000 0x2F87FF = do
    ram <- readIORef ram
    if all (flip M.member ram) addrs
      then return $ ("ram", map (ram M.!) addrs)
      else printf "Warning: uninitialized ram access: 0x%08X\n" addr >> return ("ram", zeros)
  | otherwise = return (section addr, zeros)
  where
  addrs = [addr .. addr + fromIntegral bytes - 1]
  zeros = replicate (fromIntegral bytes) 0

store' (Mem _ ram) addr bytes
  | inRange addr 0x3F8000 0x3FFFFF || inRange addr 0x2F8000 0x2F87FF = do
    r <- readIORef ram
    writeIORef ram $ foldl (\ m (addr, byte) -> M.insert addr byte m) r $ zip [addr ..] bytes
    return "ram"
  | otherwise = return $ section addr


inRange :: Word64 -> Word64 -> Word64 -> Bool
inRange addr from to = addr >= from && addr <= to

inSpace :: Word64 -> Word64 -> Word64 -> Bool
inSpace addr base length = addr >= base && addr < (base + length)

section addr
  | inSpace addr 0x30000000 16384    = "eeprom_space"
  | inSpace addr 0x00302000 2048     = "runtime_startup_stack(tpu)"
  | inSpace addr 0x00304100 256      = "tpu_a_space"
  | inSpace addr 0x00304500 256      = "tpu_b_space"
  | inSpace addr 0x00010000 0x80000  = "application"
  | otherwise                        = "unknown"
