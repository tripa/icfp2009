import Graphics.Rendering.Cairo
import Graphics.UI.Gtk hiding (fill)

import Data.Binary (decodeFile)
import Data.IORef

import Control.Monad (liftM, when)

import System.Environment (getArgs)
import System (exitWith, ExitCode(ExitSuccess))

import Orbit
import SBF
import Bin1

data Player = Player { 
      vm :: VMState
    , input :: [LogFrame]
    }

data GUI = GUI { guiDisplay :: DrawingArea
               , guiTime :: Entry
               , guiFuel :: Entry }

playerInit = do
  (filename:_) <- getArgs
  (SBF (cfg, pm)) <- decodeFile filename :: IO SBF
  newIORef (Player (initialVM initData initCode (fromIntegral cfg)) pm)

exposeCb gui s _ = do
  dw <- widgetGetDrawWindow . guiDisplay $ gui
  vm <- vm `liftM` readIORef s

  let x = -sOutput vm 2
      y = -sOutput vm 3
      r = sOutput vm 4

  renderWithDrawable dw $ do
    translate 250 250
    scale (250/1e8) (250/1e8)
    (w, _) <- deviceToUserDistance 1 0
    setLineWidth w

    -- earth
    arc 0 0 6.357e6 0 (2*pi)
    setSourceRGB 0.5 0.5 0.0
    fill

    -- target orbit
    arc 0 0 r 0 (2*pi)
    setSourceRGB 1 0 0
    stroke

    -- satellite
    arc x y w 0 (2*pi)
    setSourceRGB 0.0 0.75 0.0
    stroke

  entrySetText (guiTime gui) . show $ vmClock vm
  entrySetText (guiFuel gui) . show $ sOutput vm 1

  return True

tick (Player vm []) = Player (vmRun vm) []
tick (Player vm lf@((ts,pm):lf'))
      | vmClock vm >= ts = Player (vmRun vm { inPort = pm ++ inPort vm } ) lf'
      | otherwise        = Player (vmRun vm) lf

step s da = do
  modifyIORef s tick
  ts <- (vmClock . vm) `liftM` readIORef s
  when (ts `mod` 100 == 0) $ widgetQueueDraw da
  return True

main = do
  s <- playerInit
  initGUI 

  window <- windowNew
  set window [ windowTitle := "Orbit" ]
  v <- vBoxNew False 0
  containerAdd window v

  da <- drawingAreaNew
  widgetSetSizeRequest da 500 500
  containerAdd v da

  h <- hBoxNew False 0
  containerAdd v h
  labelNew (Just "T:") >>= containerAdd h
  time <- entryNew
  set time [ entryAlignment := 1, entryEditable := False ]
  containerAdd h time
  labelNew (Just "F:") >>= containerAdd h
  fuel <- entryNew
  set fuel [ entryEditable := False ]
  containerAdd h fuel

  let gui = GUI da time fuel

  p <- idleAdd (step s da) priorityDefaultIdle
  step s da -- iterate at least once so output ports are defined

  onDestroy window mainQuit
  onExpose da $ exposeCb gui s
  widgetShowAll window

  mainGUI

  idleRemove p
  widgetDestroy da
  widgetDestroy window
