module Main where

import Data.Bits
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
  simulate m 0 0x100 $ pack $ BS.unpack p

pack :: [Word8] -> [Word32]
pack [] = []
pack (a:b:c:d:e) = (shiftL (fromIntegral a) 24 .|. shiftL (fromIntegral b) 16 .|. shiftL (fromIntegral c) 8 .|. fromIntegral d) : pack e
pack _ = error "disassemble: byte string not divisable by 4"

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
    | otherwise = printf "load (%s) 0x%08X %d bytes\n" (section addr) addr bytes
    where
    addrs = [addr .. addr + fromIntegral bytes - 1]

  store :: IORef (M.Map Word64 Word8) -> Word64 -> [Word8] -> IO ()
  store ram addr bytes
    | addr >= 0x3F8000 && addr < 0x400000 = do
      r <- readIORef ram
      writeIORef ram $ foldl (\ m (addr, byte) -> M.insert addr byte m) r $ zip [addr ..] bytes
    | otherwise = printf "store (%s) 0x%08X 0x" (section addr) addr >> mapM_ (printf "%02X") bytes >> printf "\n"
 
  section addr
    | addr >= 0x30000000 && addr < 0x30000000 + 16384 = "EEPROM_SPACE"
    | addr >= 0x00302000 && addr < 0x00302000 + 2048  = "RUNTIME_STARTUP_STACK(TPU)"
    | addr >= 0x00304100 && addr < 0x00304100 + 256   = "TPU_A_SPACE"
    | addr >= 0x00304500 && addr < 0x00304500 + 256   = "TPU_B_SPACE"
    | otherwise                                       = "UNKNOWN"
