module SBF where

import Orbit (LogFrame, vmConfig, vmClock, vmInputLog)

import Data.Word (Word32)
import Data.List (genericLength)
import Control.Monad (forM_)
import Data.Binary (Binary, get, put)
import Data.Binary.Put (putWord32le)
import Data.Binary.IEEE754 (putFloat64le)

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

finalLog s = SBF (vmConfig s, reverse $ (vmClock s, []) : (vmInputLog s))
