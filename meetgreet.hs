{-#  LANGUAGE NoMonomorphismRestriction #-}

module Main where

import Orbit
import Bin2
import SBF (finalLog)

import Data.Word (Word16)
import Data.List (find)
import Data.Maybe (fromJust)
import Data.Binary (encodeFile)
import Control.Monad (forM_)
import Text.Printf (printf)
import System.Environment (getArgs)

type Coords = (Double, Double)

main = do
  args <- getArgs
  let cfg = (read $ args !! 0)::Word16
      s = initialVM initData initCode cfg
      v0 = vmInit s
      ss0 = satellitePos v0
  mgDump v0

  let sr = vectorLength . satellitePos $ v0
      t1 = circularOrbitPeriod (vmR v0)
      t2 = circularOrbitPeriod sr
      th = hohmannDuration (vmR v0) sr
  printf "T1=%g T2=%g Th=%g\n" t1 t2 th

  let cp = 2*pi * th / t2
      v1 = closestToWith pi (m2pi . (cp-) . thetaToSat) $ 
           iterate vmCoast v0
  putStrLn "\nWaiting for the right moment..."
  mgDump v1

  let v1' = vmThrust v1 (speedAdjust v1 $ hohmannSpeed (vmR v1) sr)
      v2 = descendingMaxWith satelliteDist $ 
           drop (floor $ th-900) $
           iterate vmCoast v1'
  putStrLn "\nGiving Hohmann thrust..."
  mgDump v1'
  putStrLn "\nCoasting..."
  mgDump v2

  let v2' = (iterate (\v -> vmThrust v (satelliteLock v)) v2 ) !! 900
      v3 = fromJust . find (\v -> vmScore v /= 0 || 
                                  vmClock(vm0 v) > vmClock(vm0 v2') + 1000) .
           iterate vmCoast $ v2'
  putStrLn "\nAdjusting..."
  mgDump v2'
  -- putStrLn "\nStabilizing..."
  -- mgDump v2''
  putStrLn "\nDone."
  mgDump v3

  encodeFile ("log" ++ (show cfg) ++ ".sbf") $ finalLog . vm0 $ v3

mgDump v = do
  putStrLn $ vmDump v
  let (sx, sy) = satellitePos v
  let (vx, vy) = satelliteSpeed v
  printf "%.4g(%.4g) %.4g(%.4g)\n" sx vx sy vy
  printf "dist=%g theta=%g\n" (satelliteDist v) (thetaToSat v)

satellitePos v = (sx, sy)
    where sx = vmX v - vmOutput v 4
          sy = vmY v - vmOutput v 5

satelliteSpeed v = (sx'-sx, sy'-sy)
    where (sx, sy) = satellitePos v
          sx' = vmX v + vmVx v - (sOutput (vm1 v) 4)
          sy' = vmY v + vmVy v - (sOutput (vm1 v) 5)

satelliteLock v = ((satellitePos v) /+ (satelliteSpeed v)) /-
                  ((vmPos v) /+ (vmSpeed v))

satelliteDist v = vectorLength (vmOutput v 4, vmOutput v 5)

thetaToSat v = atan2 s c
    where s = r1 /* r2
          c = r1 /. r2
          r1 = (vmX v, vmY v)
          r2 = satellitePos v

(x1,y1) /+ (x2,y2) = (x1+x2, y1+y2)
(x1,y1) /- (x2,y2) = (x1-x2, y1-y2)
(x1,y1) /. (x2,y2) = x1*x2 + y1*y2
(x1,y1) /* (x2,y2) = x1*y2 - x2*y1

ascendingMaxWith f (x1 : xs@(x2 : _)) | f x1 < f x2 = ascendingMaxWith f xs
                                      | otherwise   = x1
ascendingMax = ascendingMaxWith id
descendingMaxWith f = ascendingMaxWith (negate . f)
descendingMax = ascendingMaxWith negate
closestToWith t f = descendingMaxWith distToT . 
                    dropUntilDescendsWith distToT
    where distToT s = abs(t - f s)

dropUntilDescendsWith f xs@(x1:xs'@(x2:_))
    | f x1 > f x2 = xs
    | otherwise   = dropUntilDescendsWith f xs'

circularOrbitPeriod r = 2*pi * sqrt(r^^3/mu)
hohmannDuration r1 r2 = pi * sqrt((r1+r2)^^3/(8*mu))
hohmannSpeed r1 r2 = sqrt((2*mu*r2)/(r1*(r1+r2)))

speedAdjust vm vf = v' /- v
    where delta = -signum (r /* v)
          vx' = delta * y * vf / rr
          vy' = -delta * x * vf / rr
          v' = (vx', vy')
          r@(x, y) = (vmX vm, vmY vm)
          rr = vectorLength r
          v = (vmVx vm, vmVy vm)

--dmod :: Double -> Double -> Double
dmod a b = a - fromIntegral(floor(a/b))*b

m2pi = (`dmod` (2*pi))