module SBF where

import Orbit (LogFrame, vmConfig, vmClock, vmInputLog)

import Data.Word (Word32)
import Data.List (genericLength)
import Control.Monad (liftM, forM_, replicateM)
import Data.Binary (Binary, get, put)
import Data.Binary.Get (isEmpty, getWord32le)
import Data.Binary.Put (putWord32le)
import Data.Binary.IEEE754 (getFloat64le, putFloat64le)

newtype SBF = SBF (Word32, [LogFrame]) deriving Show
instance Binary SBF where
    get = getSBF
    put = putSBF

whileNotEof get = do
  e <- isEmpty
  if e then return []                                      
       else get >>= \x -> whileNotEof get >>= return . (x:)

getSBF = do
  magic <- getWord32le
  teamId <- getWord32le
  cfg <- getWord32le
  fs <- whileNotEof getLogFrame
  return . SBF $ (cfg, fs)

putSBF (SBF (cfg, fs)) = do
  putWord32le 0xCAFEBABE
  putWord32le 723
  putWord32le cfg
  forM_ fs putLogFrame

getLogFrame = do
  c <- getWord32le
  pm <- getPortMapping
  return (c, pm)

putLogFrame (c, pm) = do
  putWord32le c
  putPortMapping pm

getPortMapping = do
  l <- getWord32le
  replicateM (fromIntegral l) getPort

putPortMapping pm = do
  putWord32le $ genericLength pm
  forM_ pm putPort

getPort = do
  a <- fromIntegral `liftM` getWord32le
  v <- getFloat64le
  return (a, v)

putPort (a, v) = do
  putWord32le $ fromIntegral a
  putFloat64le v

finalLog s = SBF (vmConfig s, reverse $ (vmClock s, []) : (vmInputLog s))
