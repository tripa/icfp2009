module OBF where

import Orbit (Address, DataWord)

import Data.Word (Word32)
import Data.Bits (Bits, bit, (.|.), (.&.), shiftR)
import Control.Monad (liftM)
import Data.Binary (Binary, Get, decodeFile, get, put)
import Data.Binary.Get (isEmpty, getWord32le)
import Data.Binary.IEEE754 (getFloat64le)
import Text.Printf (printf)

type InstrWord = Word32

data Instr = DInstr DInstr | SInstr SInstr
instance Show Instr where
    show (DInstr d) = show d
    show (SInstr s) = show s

decode :: InstrWord -> Instr
decode word = if opcode /= 0
              then DInstr $ decodeD word opcode
              else SInstr $ decodeS word
    where opcode = extractOpCode word

data DInstr = Add Address Address
            | Sub Address Address
            | Mult Address Address
            | Div Address Address
            | Output Address Address
            | Phi Address Address
              deriving Show

decodeD :: InstrWord -> InstrWord -> DInstr
decodeD instr opcode = (decodeDOpCode opcode)
                       (decodeAddr . extractR1 $ instr)
                       (decodeAddr . extractR2 $ instr)

decodeDOpCode :: InstrWord -> Address -> Address -> DInstr
decodeDOpCode 1 = Add
decodeDOpCode 2 = Sub
decodeDOpCode 3 = Mult
decodeDOpCode 4 = Div
decodeDOpCode 5 = Output
decodeDOpCode 6 = Phi

data SInstr = Noop
            | Cmpz Cmp Address
            | Sqrt Address
            | Copy Address
            | Input Address
              deriving Show

data Cmp = LTZ | LEZ | EQZ | GEZ | GTZ deriving Show

decodeS :: InstrWord -> SInstr
decodeS instr = (decodeSOpCode . extractSOpCode $ instr)
                (decodeCmp . extractImm $ instr)
                (decodeAddr . extractR2 $ instr)

decodeSOpCode :: InstrWord -> Cmp -> Address -> SInstr
decodeSOpCode 0 = const $ const Noop
decodeSOpCode 1 = Cmpz
decodeSOpCode 2 = const Sqrt
decodeSOpCode 3 = const Copy
decodeSOpCode 4 = const Input

decodeCmp :: InstrWord -> Cmp
decodeCmp 0 = LTZ
decodeCmp 1 = LEZ
decodeCmp 2 = EQZ
decodeCmp 3 = GEZ
decodeCmp 4 = GTZ

decodeAddr = fromIntegral

mask l h = foldr (.|.) 0 . map bit $ [l..h]
extractBits :: (Bits n) => Int -> Int -> n -> n
extractBits l h instr = (mask l h .&. instr) `shiftR` l

extractOpCode = extractBits 28 31
extractR1 = extractBits 14 27
extractR2 = extractBits 0 13

extractSOpCode = extractBits 24 27
extractImm = extractBits 21 23

type Frame = (DataWord, InstrWord)

decodeData :: Get DataWord
decodeData = getFloat64le

decodeInstr :: Get InstrWord
decodeInstr = getWord32le

newtype EvenFrame = EvenFrame Frame deriving Show
instance Binary EvenFrame where
    put = undefined
    get = do
      dat <- decodeData
      instr <- decodeInstr
      return $ EvenFrame (dat, instr)

decodeEvenFrame :: Get Frame
decodeEvenFrame = do
  EvenFrame f <- get
  return f

newtype OddFrame = OddFrame Frame deriving Show
instance Binary OddFrame where
    put = undefined
    get = do
      instr <- decodeInstr
      dat <- decodeData
      return $ OddFrame (dat, instr)

decodeOddFrame :: Get Frame
decodeOddFrame = do
  OddFrame f <- get
  return f

newtype OBF = OBF [Frame] deriving Show
instance Binary OBF where
    put = undefined
    get = decodeOBF
liftOBF f (OBF b) = OBF (f b)

decodeOBF :: Get OBF
decodeOBF = OBF `liftM` rec decodeEvenFrame decodeOddFrame
    where rec f n = do
            done <- isEmpty
            if done 
              then return []
              else do
                head <- f
                tail <- rec n f
                return $ head:tail

decodeBinaryFile :: FilePath -> IO OBF
decodeBinaryFile file = decodeFile ("/home/tripa/work/icfp2009/" ++ file)

dumpOBF :: OBF -> IO ()
dumpOBF (OBF fs) = rec 0 fs
    where rec :: Int -> [Frame] -> IO ()
          rec _ [] = return ()
          rec c ((d,i):fs) = do
            printf "%5d %-20s [%g]\n"
                   c
                   (show . decode $ i)
                   d
            rec (succ c) fs
