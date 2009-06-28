module Main where

import Orbit
import Bin1

import Data.List (sortBy)
import Data.Maybe (fromJust)
import Data.Function (on)
import Data.Binary (encodeFile)
import Text.Printf (printf)
import System.Environment (getArgs)

main = do
  args <- getArgs
  let cfg = read $ args !! 0
      vm = initialVM initData initCode cfg
      h0 = hinit (vmRun vm)
      source = hradius h0
      target = htarget h0
      dv = sqrt(mu/source) * (sqrt(2*target/(source+target))-1)
  hdump h0
  log <- hohmann h0 $ dv
  case log of
    Just sbf -> encodeFile ("log" ++ (show cfg) ++ ".sbf") $ sbf
    Nothing -> putStrLn "Orbit miss"

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
                 (clock . vm0 $ h)

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

hohmann h dv1 = do
  let source = hradius h
      target = htarget h
  printf "Target orbit %g, sending burst (dv=%g)\n" target dv1
  let vx1 = (vx h)
      vy1 = (vy h)
      v1 = vl vx1 vy1
      dvx1 = dv1 * vx1 / v1
      dvy1 = dv1 * vy1 / v1
      h1 = hnext h dvx1 dvy1
  h2 <- simUntil (endOfBallistic source target) h1
  hdump h2
  if (hradius h2) `closeEnoughTo` target
    then hohmann2 h2
    else return Nothing

endOfBallistic source target h = hradius h `op` target || 0 `op` dr h
    where op = if target > source 
               then (>=)
               else (<=)

hohmann2 h2 = do
  let x2 = (hsx h2)
      y2 = (hsy h2)
      r2 = vl x2 y2
      vt = sqrt(mu/r2)
      vx2 = vx h2
      vy2 = vy h2
      delta = signum (x2*vy2 - y2*vx2)
      vx2' = delta * y2 * vt / r2
      vy2' = -delta * x2 * vt / r2
      dvx2 = vx2' - vx2
      dvy2 = vy2' - vy2
      c = clock . vm0 $ h2
  printf "Target orbit reached, sending burst (dv=%g)\n" (vl dvx2 dvy2)
  let h3 = hnext h2 dvx2 dvy2
  h4 <- simUntil (done $ c+1000) h3
  hdump h4
  if hscore h4 > 0
    then return . Just $ finalLog . vm0 $ h4
    else return Nothing

done c h = hscore h /= 0 || clock(vm0 h) >= c

g = 6.67428e-11
me = 6e24
mu = g*me

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
