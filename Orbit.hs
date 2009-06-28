module Orbit where

import Control.Monad
import Control.Monad.Trans
import Data.Int
import Data.Word (Word16, Word32)
import Data.Array.Unboxed
import Data.Bits
import Data.List ((\\), genericLength)
import Data.Binary hiding (decode)
import Data.Binary.Get
import Data.Binary.Put
import Data.Binary.IEEE754
import qualified Data.ByteString.Lazy as B
import Text.Printf (printf)

type Address = Word16 -- only 14 used
type DataWord = Double
type DataMem = UArray Address DataWord
type CodeMem = (DataMem, PortMapping) -> (DataMem, PortMapping)
type TimeStamp = Word32
type PortMapping = [(Address, DataWord)]
type LogFrame = (TimeStamp, PortMapping)

data VMState = VMState {
      dataMem :: DataMem
    , codeMem :: CodeMem
    , inPort :: PortMapping
    , outPort :: PortMapping
    , previousIn :: PortMapping
    , clock :: TimeStamp
    , inputLog :: [LogFrame]
    , config :: Word32
    }

-- peek :: VMState -> Address -> DataWord
-- peek s a | a <= maxAddr s = dataMem s ! a
--          | otherwise      = 0

newtype SBF = SBF (Word32, [LogFrame])
instance Binary SBF where
    get = undefined
    put (SBF (cfg,fs)) = do
      putWord32le 0xCAFEBABE
      putWord32le 723
      putWord32le cfg
      forM_ fs $ \(c, pm) -> do
                      putWord32le c
                      putWord32le (genericLength pm)
                      forM_ pm $ \(a, v) -> do
                               putWord32le . fromIntegral $ a
                               putFloat64le v

initialVM :: DataMem -> CodeMem -> Word16 -> VMState
initialVM d c cfg =
    VMState
      d
      c
      [(0x3E80, fromIntegral cfg)]
      undefined -- should be defined by vmRun
      []
      0
      []
      (fromIntegral cfg)

-- vmStep :: VMState -> VMState
-- vmStep s = ((currentCode s) s) { current = succ . current $ s }

-- currentCode :: VMState -> VMState -> VMState
-- currentCode s | a <= maxAddr s = (instrMem s) ! a
--               | otherwise      = error $ "Tried to read code " ++ show a
--     where a = current s

vmRun :: VMState -> VMState
vmRun s = l' `seq`
          s { dataMem = d'
            , outPort = o
            , previousIn = i'
            , clock = succ c
            , inputLog = l'
            }
    where (d', o) = codeMem s $ (dataMem s, i')
          i' = inPort s
          c = clock s
          l' = diffPorts c (inputLog s) (previousIn s) i' 

diffPorts c l p p' = if numChanges > 0 then (c,portChange):l else l
    where newPorts = p' \\ p
          oldPorts = zip ((map fst p) \\ (map fst p')) (repeat 0)
          portChange = newPorts ++ oldPorts
          numChanges = length portChange

finalLog s = SBF (config s, reverse $ (clock s, []) : (inputLog s))

-- eval :: InstrWord -> VMState -> VMState
-- eval i = case decode i of
--            DInstr d -> evalD d
--            SInstr s -> evalS s

-- evalD :: DInstr -> VMState -> VMState
-- evalD (Add r1 r2) = binary (+) r1 r2
-- evalD (Sub r1 r2) = binary (-) r1 r2
-- evalD (Mult r1 r2) = binary (*) r1 r2
-- evalD (Div r1 r2) = binary (//) r1 r2
--     where _  // 0.0 = 0.0
--           r1 // r2  = r1 / r2
-- evalD (Output r1 r2) = \s -> s { outPort = (r1,peek s r2):outPort s }
-- evalD (Phi r1 r2) = \s -> updateMem s $ if flag s
--                                         then peek s r1
--                                         else peek s r2

-- updateMem :: VMState -> DataWord -> VMState
-- updateMem s v | a <= maxAddr s = s { dataMem = dataMem s // [(a, v)] }
--               | otherwise      = error $ "Tried to write data " ++ show a
--     where a = current s

-- binary :: (DataWord -> DataWord -> DataWord) -> Address -> Address
--        -> VMState -> VMState
-- binary op r1 r2 s = updateMem s $ op (peek s r1) (peek s r2)

-- evalS :: SInstr -> VMState -> VMState
-- evalS Noop = id
-- evalS (Cmpz c r1) = cmpz c r1
-- evalS (Sqrt r1) = unary sqrt r1
-- evalS (Copy r1) = unary id r1
-- evalS (Input r1) = input r1

-- cmpz :: Cmp -> Address -> VMState -> VMState
-- cmpz cmp r1 s = s { flag = peek s r1 `op` 0.0 }
--     where op :: (Ord a) => a -> a -> Bool
--           op = case cmp of
--                  LTZ -> (<)
--                  LEZ -> (<=)
--                  EQZ -> (==)
--                  GEZ -> (>=)
--                  GTZ -> (>)

-- input :: Address -> VMState -> VMState
-- input r1 s = updateMem s . maybe 0 id . lookup r1 $ inPort s

-- unary :: (DataWord -> DataWord) -> Address -> VMState -> VMState
-- unary op r s = updateMem s $ op (peek s r)
