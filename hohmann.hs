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
  let vm = initialVM initData initCode cfg
  hohmann vm

vl x y = sqrt ((x*x) + (y*y))
measure n = fromJust . lookup n . outPort
hscore = measure 0 . vm0
hfuel = measure 1 . vm0
hsx = measure 2 . vm0
hsy = measure 3 . vm0
hto = measure 4 . vm0

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
                  | n == 0    = hdump h >> rec 250 h
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

hohmann vm = do
  let h0 = hinit (vmRun vm)
      sx0 = hsx h0
      sy0 = hsy h0
      source = hradius h0
      target = hto h0
      passed = if target > source then (<= 0) . dr else (>= 0) . dr
      vx0 = vx h0
      vy0 = vy h0
      v0 = vl vx0 vy0
  hdump h0
  let dv1 = sqrt(mu/source) * (sqrt(2*target/(source+target))-1)
      dvx1 = dv1 * vx0 / v0
      dvy1 = dv1 * vy0 / v0
  printf "Target orbit %g, sending burst (dv=%g)\n" target dv1
  let h1 = hnext h0 dvx1 dvy1
  h2 <- simUntil passed h1
  hdump h2
  let target' = hradius h2
      vt = sqrt(mu/target')
      vx2 = vx h2
      vy2 = vy h2
      v2 = vl vx2 vy2
      dvx2 = vt * vx2 / v2 - vx2
      dvy2 = vt * vy2 / v2 - vy2
  printf "Target orbit reached, sending burst (dv=%g)\n" (vl dvx2 dvy2)
  let h3 = hnext h2 dvx2 dvy2
  h4 <- simUntil ((/= 0) . hscore) h3
  hdump h4
  putStrLn "Dumping log to log.sbf"
  encodeFile "log.sbf" $ finalLog . vm0 $ h4

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
