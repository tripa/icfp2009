import Control.Monad
import Control.Monad.Trans
import Data.Int
import Data.Word
import Data.Array.Unboxed
import Data.Bits
import Data.List
import Data.Maybe
import Data.Binary hiding (decode)
import Data.Binary.Get
import Data.Binary.Put
import Data.Binary.IEEE754
import qualified Data.ByteString.Lazy as B
import System.Environment
import Text.Printf

type Address = Word16 -- only 14 used
type DataWord = Double
type InstrWord = Word32
type TimeStamp = Word32
type PortMapping = [(Address, DataWord)]
type LogFrame = (TimeStamp, PortMapping)

data VMState = VMState {
      maxAddr :: Address
    , current :: Address
    , flag :: Bool
    , dataMem :: UArray Address DataWord
    , instrMem :: Array Address (VMState -> VMState)
    , inPort :: PortMapping
    , outPort :: PortMapping
    , previousIn :: PortMapping
    , clock :: TimeStamp
    , inputLog :: [LogFrame]
    , config :: Word32
    }

peek :: VMState -> Address -> DataWord
peek s a | a <= maxAddr s = dataMem s ! a
         | otherwise      = 0

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

initialVM :: OBF -> Word16 -> VMState
initialVM (OBF fs) cfg =
      VMState
      m
      undefined -- should be defined by vmRun
      False
      (accumArray (curry snd) 0 (0,m) $ zip [0..] dataStream)
      (accumArray (curry snd) id (0,m) $ zip [0..] $ map eval $ instrStream)
      [(0x3E80, fromIntegral cfg)]
      undefined -- should be defined by vmRun
      []
      0
      []
      (fromIntegral cfg)
    where (dataStream, instrStream) = unzip fs
          m = pred . genericLength $ fs

vmStep :: VMState -> VMState
vmStep s = ((currentCode s) s) { current = succ . current $ s }

currentCode :: VMState -> VMState -> VMState
currentCode s | a <= maxAddr s = (instrMem s) ! a
              | otherwise      = error $ "Tried to read code " ++ show a
    where a = current s

vmRun :: VMState -> VMState
vmRun s = l' `seq`
          s' { previousIn = i'
             , clock = succ c
             , inputLog = l'
             }
    where s' = fromJust $
               find done $
               iterate vmStep s { current = 0, outPort = [] }
          done s = (current s) > (maxAddr s)
          i' = inPort s
          c = clock s
          l' = diffPorts c (inputLog s) (previousIn s) i' 

diffPorts c l p p' = if numChanges > 0 then (c,portChange):l else l
    where newPorts = p' \\ p
          oldPorts = zip ((map fst p) \\ (map fst p')) (repeat 0)
          portChange = newPorts ++ oldPorts
          numChanges = length portChange

finalLog s = SBF (config s, reverse $ (clock s, []) : (inputLog s))

eval :: InstrWord -> VMState -> VMState
eval i = case decode i of
           DInstr d -> evalD d
           SInstr s -> evalS s

evalD :: DInstr -> VMState -> VMState
evalD (Add r1 r2) = binary (+) r1 r2
evalD (Sub r1 r2) = binary (-) r1 r2
evalD (Mult r1 r2) = binary (*) r1 r2
evalD (Div r1 r2) = binary (//) r1 r2
    where _  // 0.0 = 0.0
          r1 // r2  = r1 / r2
evalD (Output r1 r2) = \s -> s { outPort = (r1,peek s r2):outPort s }
evalD (Phi r1 r2) = \s -> updateMem s $ if flag s
                                        then peek s r1
                                        else peek s r2

updateMem :: VMState -> DataWord -> VMState
updateMem s v | a <= maxAddr s = s { dataMem = dataMem s // [(a, v)] }
              | otherwise      = error $ "Tried to write data " ++ show a
    where a = current s

binary :: (DataWord -> DataWord -> DataWord) -> Address -> Address
       -> VMState -> VMState
binary op r1 r2 s = updateMem s $ op (peek s r1) (peek s r2)

evalS :: SInstr -> VMState -> VMState
evalS Noop = id
evalS (Cmpz c r1) = cmpz c r1
evalS (Sqrt r1) = unary sqrt r1
evalS (Copy r1) = unary id r1
evalS (Input r1) = input r1

cmpz :: Cmp -> Address -> VMState -> VMState
cmpz cmp r1 s = s { flag = peek s r1 `op` 0.0 }
    where op :: (Ord a) => a -> a -> Bool
          op = case cmp of
                 LTZ -> (<)
                 LEZ -> (<=)
                 EQZ -> (==)
                 GEZ -> (>=)
                 GTZ -> (>)

input :: Address -> VMState -> VMState
input r1 s = updateMem s . maybe 0 id . lookup r1 $ inPort s

unary :: (DataWord -> DataWord) -> Address -> VMState -> VMState
unary op r s = updateMem s $ op (peek s r)

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

main = do
  args <- getArgs
  let filename = args !! 0
      cfg = read $ args !! 1
  obf <- decodeBinaryFile filename
  let vm = initialVM obf cfg
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
  printf "s(%g,%g) T=%g\n" sx0 sy0 target
  hdump h0
  printf "v(%g,%g) V=%g\n" vx0 vy0 v0
  let dv1 = sqrt(mu/source) * (sqrt(2*target/(source+target))-1)
      dvx1 = dv1 * vx0 / v0
      dvy1 = dv1 * vy0 / v0
  printf "Sending burst (dv=%g)\n" dv1
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
  printf "Reached target orbit, burst (dv=%g)\n" (vl dvx2 dvy2)
  let h3 = hnext h2 dvx2 dvy2
  h4 <- simUntil ((/= 0) . hscore) h3
  hdump h4
  putStrLn "Dumping log to log.sbf"
  encodeFile "log.sbf" $ finalLog . vm0 $ h4

g = 6.67428e-11
me = 6e24
mu = g*me
