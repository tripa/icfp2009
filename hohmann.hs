module Main where

import Orbit hiding (vm0, vm1, defaultIn)
import Bin1
import SBF (finalLog)

import Data.List (sortBy)
import Data.Maybe (fromJust, isNothing)
import Data.Function (on)
import Control.Monad (liftM)
import Data.Binary (encodeFile)
import Text.Printf (printf)
import System.Environment (getArgs)

main = do
  args <- getArgs
  let cfg = read $ args !! 0
      vm = initialVM initData initCode cfg
      h0 = hinit (vmRun vm)
  hdump h0
  hf <- fromJust `liftM` startHohmann h0
  let rf = hfuel hf
  printf "Remaining fuel: %g\n" rf
  hf' <- bsearch hscore (wastingHohmann h0) (hscore hf) 0 rf
  encodeFile ("log" ++ (show cfg) ++ ".sbf") $ finalLog . vm0 $ hf'

wastingHohmann :: Hohmann -> Double -> IO (Maybe Hohmann)
wastingHohmann h0 w = do
  let (dvx, dvy) = adjustSpeed (w + hohmannSpeed h0) (hpos h0) (hspeed h0)
      h1 = hnext h0 dvx dvy
  startHohmann h1

bsearch ev f best good bad = do
  let value = (good+bad) / 2
  result <- f value
  let tested = fromJust result
      worth  = ev tested
  bsearch' ev f best good bad value result tested worth

bsearch' ev f best good bad value result tested worth
    | isNothing result         = bsearch ev f best good value
    | worth < best             = bsearch ev f best good value
    | abs(worth - best) < 1e-6 = return tested
    | otherwise                = bsearch ev f worth value bad

vl x y = sqrt ((x*x) + (y*y))
measure n = fromJust . lookup n . outPort
hscore = measure 0 . vm0
hfuel = measure 1 . vm0
hsx = measure 2 . vm0
hsy = measure 3 . vm0
htarget = measure 4 . vm0

hradius vm = vl (hsx vm) (hsy vm)

hdump h = printf "F=%5.2g R=%.6e V=%5.2f S=%g T=%d\n"
                 (hfuel h)
                 (hradius h)
                 (vl (vx h) (vy h))
                 (hscore h)
                 (vmClock . vm0 $ h)

closeEnoughTo d = (< 1000) . abs . (d-)

simUntil f h = rec 0 h
    where rec n h | f h       = return h
                  | n == 0    = hdump h >> rec 5000 h
                  | otherwise = rec (pred n) $ hnext h 0 0

simCorrectUntil d c f h = rec 0 h
    where rec n h | f h       = return h
                  | n == 0    = hdump h >> rec 10 h
                  | otherwise = rec (pred n) $ hnext h (dvx/d) (dvy/d)
              where (dvx, dvy) = c h

hcorrect vt h = (dvx, dvy)
    where cvx = vx h
          cvy = vy h
          cv  = vl cvx cvy
          dvx = vt * cvx / cv - cvx
          dvy = vt * cvy / cv - cvy

data Hohmann = Hohmann { vm0 :: VMState
                       , vm1 :: VMState
                       , vx :: Double
                       , vy :: Double
                       , dr :: Double
                       , defaultIn :: PortMapping }

hinit :: VMState -> Hohmann
hinit s = hstep s (inPort s)

hnext :: Hohmann -> Double -> Double -> Hohmann
hnext h 0 0 = hstep (vm1 h) (defaultIn h)
hnext h dvx dvy = hstep (vmRun ((vm0 h) { inPort = (2,-dvx):(3,-dvy):i })) i
    where i = defaultIn h

hstep :: VMState -> PortMapping -> Hohmann
hstep s i = Hohmann s s' vx vy dr i
    where s' = vmRun s { inPort = i }
          x0 = measure 2 s
          y0 = measure 3 s
          x1 = measure 2 s'
          y1 = measure 3 s'
          vx = x1 - x0
          vy = y1 - y0
          dr = (vl x1 y1) - (vl x0 y0)

adjustSpeed v (x, y) (vx, vy) = (vx' - vx, vy' - vy)
    where delta = -signum (x*vy - y*vx)
          vx' =  delta * y * v / r
          vy' = -delta * x * v / r
          r = vl x y

hpos h = (hsx h, hsy h)
hspeed h = (vx h, vy h)

hohmannSpeed h = sqrt((2*mu*r2)/(r1*(r1+r2)))
    where r1 = hradius h
          r2 = htarget h

startHohmann h0 = do
  let v = hohmannSpeed h0
      r1 = hradius h0
      r2 = htarget h0
      (dvx, dvy) = adjustSpeed v (hpos h0) (hspeed h0)
  printf "Target orbit %g, sending burst (dv=%g)\n" r2 (vl dvx dvy)
  let h1 = hnext h0 dvx dvy
  h2 <- simUntil (endOfBallistic r1 r2) h1
  hdump h2
  if (hradius h2) `closeEnoughTo` r2
    then stabilizeHohmann h2
    else return Nothing

endOfBallistic source target h = hradius h `op` target || 0 `op` dr h
    where op = if target > source 
               then (>=)
               else (<=)

stabilizeHohmann h2 = do
  let r2 = hradius h2
      vt = sqrt(mu/r2)
      (dvx, dvy) = adjustSpeed (-vt) (hpos h2) (hspeed h2)
      c = vmClock . vm0 $ h2
  printf "Target orbit reached, sending burst (dv=%g)\n" (vl dvx dvy)
  let h3 = hnext h2 dvx dvy
  h4 <- simUntil (done $ c+1000) h3
  hdump h4
  if hscore h4 > 0
    then return . Just $ h4
    else return Nothing

done c h = hscore h /= 0 || vmClock(vm0 h) >= c

-- I'm too tired to solve 2-vars quadratics in O(1)
circleInter (x, y) d r = bisect (eval a) a b
    where [a,b] = take 2 $
                  sortBy (compare `on` eval)
                         [(d,0),(-d,0),(0,d),(0,d)]
          eval (dx,dy) = abs(vl (x+dx) (y+dy) - r)
          combine (x1,y1) (x2,y2) = (x'*d/n,y'*d/n)
              where x' = x1+x2
                    y' = y1+y2
                    n = vl x' y' -- this could be 0! BUG
          bisect ea a b | ec < 1000 = c
                        | ec < ea   = bisect ec c a
                        | otherwise = bisect ea a c
              where ec = eval c
                    c = combine a b
