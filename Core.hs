{-# language DataKinds #-}
{-# language OverloadedLabels #-}
{-# language KindSignatures #-}
{-# language TypeOperators #-}
{-# language MultiParamTypeClasses #-}
{-# language FlexibleInstances #-}
{-# language TypeFamilies #-}
{-# language FlexibleContexts #-}
{-# language PolyKinds #-}
{-# language UndecidableInstances #-}
{-# language ScopedTypeVariables #-}
{-# language Arrows #-}
module Core where

import Color (HSL(..), RGB(..), hsl2rgb, rgb2hsl, rgb_d2w)
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TMVar (TMVar, newEmptyTMVar, putTMVar, takeTMVar, tryTakeTMVar)
import Control.Concurrent.STM.TQueue (TQueue, newTQueue, writeTQueue, readTQueue)
import Control.Exception (bracket, bracket_)
import Control.Monad (foldM)
import Control.Monad.Trans (MonadIO(..), lift)
import Control.Wire hiding ((<>))
import Control.Wire.Core
import Control.Wire.Unsafe.Event
import Control.Wire.Switch
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Char (chr)
import Data.Function (on)
import Data.Foldable       (asum, foldMap)
import Data.List (sort, maximum, groupBy)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Sequence (Seq, (<|), ViewR(..), viewr)
import qualified Data.Sequence as Seq
import Data.Traversable (mapAccumR, mapM)
import Data.Monoid ((<>))
import Data.Word (Word8, Word16)
import Data.Proxy (Proxy(..))
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Data.Vector.Mutable (IOVector, write)
import qualified Data.Vector.Mutable as MVector
import FRP.Netwire.Analyze (lAvg)
import FRP.Netwire.Move (derivative, integral)
import FRP.Netwire.Noise (noiseR)
import GHC.TypeLits -- (TypeError, ErrorMessage(..), Nat, Symbol, KnownNat(..), natVal, natVal')
import GHC.Exts
import Prelude hiding ((.), id, until, mapM)
import System.Environment (getArgs)
import System.Hardware.Serialport as Serialport (CommSpeed(CS9600, CS115200), SerialPort, SerialPortSettings(commSpeed), closeSerial, defaultSerialSettings, openSerial, recv, send, flush)
import System.MIDI as MIDI
import System.MIDI.Utility as MIDI
import System.Random (Random, StdGen, mkStdGen, randomR)

withSerial :: FilePath -> CommSpeed -> (SerialPort -> IO a) -> IO a
withSerial device speed f =
  bracket (openSerial device defaultSerialSettings { commSpeed = speed }) (\p -> putStrLn "Closing serial port." >> closeSerial p >> putStrLn "closed.") f

-- | COBS
-- FIXME: handle longer than 254
-- FIXME: handle 0xFF and non-zero termination
stuffBS :: ByteString -> ByteString
stuffBS b | B.length b > 254 = error "stuff not implemented for ByteStrings longer than 254 bytes. Submit a patch!"
stuffBS b = (B.cons 0 (stuff' (B.snoc b 0)))
  where
    stuff' :: ByteString -> ByteString
    stuff' b | B.null b = b
    stuff' b =
      case B.span (/= 0) b of
        (b0, b1) ->
          (B.cons (fromIntegral $ B.length b0 + 1) b0) <> (stuff' $ B.drop 1 b1)

-- seems hackish
stuff :: [Word8] -> [Word8]
stuff b | length b > 254 = error "stuff not implemented for ByteStrings longer than 254 bytes. Submit a patch!"
stuff b =
  let stuffed = (stuff' b)
  in stuffed ++ [0]
  where
    stuff' :: [Word8] -> [Word8]
    stuff' b | null b = b
    stuff' b =
      case span (/= 0) b of
        (b0, b1) ->
          case b1 of
            [0] -> ((fromIntegral $ length b0 + 1) : b0 ++ [1])
            _ -> ((fromIntegral $ length b0 + 1) : b0) <> (stuff' $ drop 1 b1)

data Parameter
  = Red
  | Green
  | Blue
  | Amber
  | White
  | UV
  | Master
  | SP64ColorMacros
  | SP64SpeedStrobe
  | S64Mode
  | GBParCon
  | GBDerbyCon
  | GBDerbyStrobe
  | GBDerbyRotation
  | GBLaserColor
  | GBLaserStrobe
  | GBLaserPattern
  | GBStrobePattern
  | GBStrobeDimmer
  | GBStrobeSpeed
  deriving (Eq, Ord, Read, Show)

type Word9 = Word16
type Address = Word9
type Value = Word8

red :: Value -> Param Red
red v = Param v

green :: Value -> Param Green
green v = Param v

blue :: Value -> Param Blue
blue v = Param v

amber :: Value -> Param Amber
amber = Param

white :: Value -> Param White
white = Param

rgbWhite :: Value -> Param 'Red :+: (Param 'Green :+: Param 'Blue)
rgbWhite v =
  red v :+: green v :+: blue v

-- orange :: [Param Red, Param Green, Param Blue]
orange :: Param 'Red :+: (Param 'Green :+: Param 'Blue)
orange = red 244 :+: green 164 :+: blue 66

uv :: Value -> Param UV
uv = Param

master :: Value -> Param Master
master v = Param v



-- | HSL to RGB
-- h = 0 to 360
-- s = 0 to 1
-- l = 0 to 1
hsl :: HSL Double -> Param 'Red :+: (Param 'Green :+: Param 'Blue)
hsl v =
  case rgb_d2w (hsl2rgb v) of
    (RGB r g b) -> red r :+: green g :+: blue b

-- LFO

data Waveform
  = Sine
  | Tri
  | Square
  | Ramp
  | PWM Int
    deriving (Eq, Show)

lfo :: Waveform -> Int -> Double -> MidiWire a Double
lfo Sine period phase = -- FIMXE should be from 0 to 1, not -1 to 1
  let i = (2*pi) / (fromIntegral period)
  in proc _ ->
      do t <- time -< () -- time in ticks, 96 ticks per measure
         returnA -< sin ((i * fromIntegral t) + phase)
lfo Ramp period phase = -- FIXME phase
  proc _ ->
    do t <- time -< ()
       returnA -< 1 - ((fromIntegral (t `mod` period)) / fromIntegral (period - 1))
lfo (PWM onTime) period _ = -- FIXME phase
  let loop =
        (for (onTime + 1) . pure 1) --> (for ((period - onTime) + 1) . pure 0) --> loop
  in loop

delay' :: Int -> Double -> MidiWire Output Output
delay' dur feedback = loop $ initDelay >>> go
  where
    mapParam :: (Word8 -> Word8) -> Param p -> Param p
    mapParam f (Param v) = (Param (f v))

    fdbk :: (Address, Word8) -> (Address, Word8)
    fdbk (addr, v) = (addr, round ((fromIntegral v) * feedback))

    initDelay :: MidiWire (Output, Seq Output) (Output, Seq Output)
    initDelay = second (delay (Seq.replicate dur []))

    go :: MidiWire (Output, Seq Output)  (Output, Seq Output)
    go =
      (proc (params, mem) ->
         returnA -<
           case viewr mem of
             (mem' :> delayedParams) ->
               let params' = mergeParams params delayedParams
                   mem'' = (map fdbk params') <| mem'
               in (params', mem'')) --> go

delay'' :: Int -> Double -> MidiWire Output Output
delay'' dur feedback = loop $ initDelay >>> go
  where
    mapParam :: (Word8 -> Word8) -> Param p -> Param p
    mapParam f (Param v) = (Param (f v))

    fdbk :: (Address, Word8) -> (Address, Word8)
    fdbk (addr, v) = (addr, round ((fromIntegral v) * feedback))

    initDelay :: MidiWire (Output, Seq Output) (Output, Seq Output)
    initDelay = second (delay (Seq.replicate dur []))

    go :: MidiWire (Output, Seq Output)  (Output, Seq Output)
    go =
      (proc (params, mem) ->
         returnA -<
           case viewr mem of
             (mem' :> delayedParams) ->
               let params' = {- mergeParams params -} delayedParams
                   mem'' = (map fdbk params) <| mem'
               in (params', mem'')) --> go

randomD :: (Random a) => StdGen -> (a, a) -> MidiWire x a
randomD initGen range =
  (loop $ proc (_, gen) ->
     do g <- delay initGen -< gen
        let (d, gen') = randomR range g
        returnA -< (d, gen'))

data Labeled (name :: Symbol) a = Labeled a

data Indexed (i :: Nat) a = Indexed [a]
                            deriving Show

data Mirrored a = Mirrored a

mirrored :: Proxy a -> Proxy (Mirrored a)
mirrored _ = Proxy

instance (KnownSymbol name, Show a) => Show (Labeled name a) where
  show (Labeled u) = "Labeled \"" ++ symbolVal (Proxy :: Proxy name) ++ "\" (" ++ show u ++ ")"

data Fixture (params :: [Parameter]) = Fixture
 { address :: Address }
   deriving (Eq, Ord, Read, Show)

data a :+: b =
  a :+: b
   deriving (Eq, Ord, Read, Show)
infixr :+:

type Frame = IOVector Value

data Param (a :: Parameter) = Param Value

class ShowParameter p where
  showParameter :: Param p -> String

instance ShowParameter Red where showParameter _ = show Red
instance ShowParameter Blue where showParameter _ = show Blue
instance ShowParameter Green where showParameter _ = show Green
instance ShowParameter Amber where showParameter _ = show Amber
instance ShowParameter White where showParameter _ = show White
instance ShowParameter UV where showParameter _ = show UV
instance ShowParameter Master where showParameter _ = show Master

instance (ShowParameter p) => Show (Param p) where
  show p@(Param v) = "Param { " ++ showParameter p  ++ " = " ++ show v ++ " }"


type family Member (a :: k) (l :: [k]) :: Bool where
  Member a '[] = False
  Member a (a ': bs) = True
  Member a (b ': bs) = Member a bs

type family HasElem (a :: k) (l :: [k]) :: Constraint where
  HasElem a l = (Member a l ~ True)

class ParamIndex (a :: Parameter) l where
  paramIndex :: Param a -> l -> Int

instance (Member a as ~ False) => ParamIndex a (Fixture (a ': as)) where
  paramIndex _ _ = 0

instance forall a b bs. (Member a bs ~ True, ParamIndex a (Fixture bs)) => ParamIndex a (Fixture (b ': bs)) where
  paramIndex a (Fixture address) = succ (paramIndex a ((Fixture address) :: Fixture bs))

-- instance forall a b bs. (Member a bs ~ True, ParamIndex a (Fixture bs)) => ParamIndex a (Fixture (b ': bs)) where

dmxBaud = CS115200
dmxPort = "/dev/cu.usbmodem836131"


sendAll :: SerialPort
        -> ByteString
        -> IO ()
sendAll port bs
  | B.null bs = pure ()
  | otherwise =
     do sent <- Serialport.send port bs
--        putStrLn $ "sent = " ++ show sent
        if (sent < B.length bs)
          then sendAll port (B.drop sent bs)
          else pure ()

-- various input events
data Input
    = Tick
    | ME MidiEvent

data OutputEvent
  = Print String
  | F [(Address, Word8)]
    deriving Show

type Output = [(Address, Word8)]
type MidiTimed   = Timed Int ()
type MidiSession m = Session m MidiTimed
type MidiWire    = Wire MidiTimed () Identity
type MidiLights  = Wire MidiTimed () Identity (Event Input) Output


type Midi2Seq = Map Int MidiLights

foldLights :: [MidiLights] -> MidiLights
foldLights [] = arr (const [])
foldLights (m:ms) = (m &&& foldLights ms) >>> (arr $ uncurry mergeParams)
{-
updateMap :: Midi2Seq -> MidiWire (Event Input) (Event Input, Event Midi2Seq)
updateMap m2s =
  proc e ->
    do let m2s' = case e of
               (Event (ME (MidiEvent _time midiMessage))) ->
                 case midiMessage of
                   (MidiMessage _channel mm) ->
                     case mm of
                       (NoteOn key vel) ->
                         case key of
                           0 -> Map.insert 0 redBlue m2s
                           1 -> Map.insert 1 strobe m2s
       returnA -< (e, Event m2s')
-}
midiModes :: Map Int MidiLights -> MidiLights
midiModes midiModes = loop (Map.fromList [])
    where
    loop ms'' =
        WGen $ \ds mxev' ->
            case mxev' of
              Left _ -> do
                let ms' = ms''
                msx <- mapM (\(k,w') -> stepWire w' ds mxev' >>= \w -> pure (k,w)) (Map.toAscList ms')
                let mergeE (Left l) (Left _) = Left l
                    mergeE (Left _) (Right p) = Right p
                    mergeE (Right p) (Left _) = Right p
                    mergeE (Right p1) (Right p2) = Right $ mergeParams p1 p2
                    (mx, ws) = mapAccumR (\e (k, (e', w)) -> (mergeE e e', (k, w))) (Right []) msx
--                  (mx, w) <- mapAccumR (\mx w'' -> stepWire w'' ds mxev'
                return (mx, loop (Map.fromList ws))
              Right ev -> do
                let ms' = switch ms'' ev
                msx <- mapM (\(k,w') -> stepWire w' ds mxev' >>= \w -> pure (k,w)) (Map.toAscList ms')
                let mergeE (Left l) (Left _) = Left l
                    mergeE (Left _) (Right p) = Right p
                    mergeE (Right p) (Left _) = Right p
                    mergeE (Right p1) (Right p2) = Right $ mergeParams p1 p2
                    (mx, ws) = mapAccumR (\e (k, (e', w)) -> (mergeE e e', (k, w))) (Right []) msx
--                  (mx, w) <- mapAccumR (\mx w'' -> stepWire w'' ds mxev'
                return (mx, loop (Map.fromList ws))

    switch ms' NoEvent = ms'
    switch ms' (Event input) =
      let ms =
            case input of
              (ME (MidiEvent _time midiMessage)) ->
                case midiMessage of
                  (MidiMessage _channel mm) ->
                    case mm of
                      (NoteOn key vel) ->
                        case Map.lookup key midiModes of
                          Nothing -> ms'
                          (Just m) -> Map.insert key m ms'
{-
                        case key of
                          0 -> (Map.insert key redBlue ms')
                          1 -> (Map.insert key strobe ms')
                          2 -> (Map.insert key gbDerbys ms')
                          3 -> (Map.insert key allRedBlue ms')
                          4 -> (Map.insert key gbStrobes1 ms')
                          5 -> (Map.insert key gbStrobes1' ms')
                          _ -> ms'
-}
                      (NoteOff key vel) -> Map.delete key ms'
                      _ -> ms'
                  _ -> ms'
              _ -> ms'
      in ms

mergeOutput :: Event [OutputEvent] -> Event [OutputEvent] -> Event [OutputEvent]
mergeOutput NoEvent e = e
mergeOutput e NoEvent = e
mergeOutput (Event e1) (Event e2) = Event $ mergeOutput' e1 e2

mergeOutput' :: [OutputEvent] -> [OutputEvent] -> [OutputEvent]
mergeOutput' events1 events2 =
  let prints = [ Print str | Print str <- events1 ] ++ [ Print str | Print str <- events2 ]
      fs     = F $ map  maximum $ groupBy ((==) `on` fst) $ sort $ concat $ [ frame | F frame <- events1 ] ++ [ frame | F frame <- events2 ]
  in (fs : (Print (show fs)) : prints)

mergeParams :: [(Address, Word8)] -> [(Address, Word8)] -> [(Address, Word8)]
mergeParams params1 params2 =
  map maximum $ groupBy ((==) `on` fst) $ sort $ params1 ++ params2

mergeParamsL :: [[(Address, Word8)]] -> [(Address, Word8)]
mergeParamsL params =
    map maximum $ groupBy ((==) `on` fst) $ sort $ concat params


thirtysecondth, sixteenth, eighth, quarter, whole :: (Num a) => a
thirtysecondth  = 3
sixteenth  = 6
eighth     = 12
quarter    = 24
half       = 48
whole      = 96

dotted :: Int -> Int
dotted n = n + (n `div` 2)

triplet :: Int -> Int
triplet n = (n * 2) `div` 3

beatSession :: (MonadIO m) => MidiSession m
beatSession =
  Session $ do
--    p0 <- liftIO $ atomiclly $ waitN pos 1
    return (Timed 0 (), loop)
  where
    loop =
      Session $ do
--        dp <- liftIO $ atomically $ waitN' pos 1
--        liftIO $ print dp
--        let dp = p - p'
        return (Timed 1 (), loop)

printOutput :: (MonadIO m) => OutputEvent -> m ()
printOutput (Print str) = liftIO $ putStrLn str
printOutput (F frame) = liftIO $ print frame

serialOutput :: (MonadIO m) => SerialPort -> Output -> m ()
serialOutput port params = liftIO $
{-
  case outputEvent of
    (Print str) -> putStrLn str
    (F params)   ->
-}
      do -- print params
         frame <- MVector.replicate (7+7+18+20) 0
         mapM_ (\(addr, val) -> write frame (fromIntegral (addr - 1)) val) params
         vals <- Vector.toList <$> Vector.freeze frame
         -- print vals
         -- print vals
         -- print (B.length (B.pack $ stuff vals))
         -- print (stuff vals)

         sendAll port (B.pack $ stuff vals)
--         Serialport.flush port
--         bs <- Serialport.recv port 100
--         putStrLn $ "recv: " ++ show bs

         pure ()

runShow :: (MonadIO m) =>
           TQueue Input
        -> (Output -> m ())
        -> MidiSession m
        -> MidiLights
        -> m a
runShow midi output s0 w0 = loop s0 w0
  where
    loop s' w' = do
      m <- liftIO $ atomically $ readTQueue midi
--      liftIO $ print m
      (ds, s) <-
        case m of
          (ME (MidiEvent _time SRTClock)) -> stepSession s'
          _         -> do -- liftIO $ print m
                          return (Timed 0 (), s')
      let Identity (mx, w) = stepWire w' ds (Right $ Event m)
--      (mx, w) <- liftIO $ stepWire w' ds (Right $ Event m)
      case mx of
        (Right out) ->
          do output out
        _                 -> return ()
      loop s w

callback :: TQueue Input -> MidiEvent -> IO ()
callback queue midiEvent =
  do -- print midiEvent
     atomically $ writeTQueue queue (ME midiEvent)

data Path
  = Here
  | Label Symbol
  | At Nat
  | P [Path]

at0 :: Proxy (At 0)
at0 = Proxy

at1 :: Proxy (At 1)
at1 = Proxy

at2 :: Proxy (At 2)
at2 = Proxy

at3 :: Proxy (At 3)
at3 = Proxy

at4 :: Proxy (At 4)
at4 = Proxy

at5 :: Proxy (At 5)
at5 = Proxy

type family ParamNat param params where
  ParamNat (Proxy a) (Proxy (a ': bs)) = 0
  ParamNat (Proxy a) (Proxy (b ': bs)) = 1 + ParamNat (Proxy a) (Proxy bs)

type family SubUniverse (path :: Path) universe :: * where
  SubUniverse (Label lbl) (Labeled lbl universe) = universe
  SubUniverse (Label lbl) (Labeled lbl universe :+: universes) = universe
  SubUniverse (Label lbl) (universe :+: universes) = SubUniverse (Label lbl) universes
  SubUniverse (At n) (Indexed m universe) = universe
  SubUniverse (At n) (Mirrored (Indexed m universe)) = universe
  SubUniverse (P '[]) universe = universe
  SubUniverse (P (p : '[])) universe = (SubUniverse p universe)
  SubUniverse (P (p : ps)) universe = SubUniverse (P ps) (SubUniverse p universe)

type family HasLabel lbl universe :: Bool where
  HasLabel lbl (Labeled lbl u) = True
  HasLabel lbl u = False

class Select (path :: Path) (universe :: *) where
  select :: Proxy path -> universe -> SubUniverse path universe

instance Select (Label lbl) (Labeled lbl universe) where
  select _ (Labeled u) = u

instance {-# OVERLAPPING #-}  Select (Label lbl) (Labeled lbl universe :+: universes) where
  select _ (Labeled u :+: universes) = u

instance {-# OVERLAPPING #-}
         ( HasLabel lbl universe ~ False
         , Select (Label lbl) universes
         , SubUniverse (Label lbl) (universe :+: universes) ~ SubUniverse (Label lbl) universes) =>
         Select (Label lbl) (universe :+: universes) where
  select lbl (universe :+: universes) = select lbl universes

instance (SubUniverse (P '[]) universe ~ universe) => Select (P '[]) universe where
  select _ u = u

instance ( Select p universe
         , Select (P ps) (SubUniverse p universe)
         , SubUniverse ('P ps) (SubUniverse p universe) ~ SubUniverse ('P (p : ps)) universe
         ) =>
  Select (P (p : ps)) universe where
  select _ universe = select (Proxy :: Proxy (P ps)) (select (Proxy :: Proxy p) universe)

instance (KnownNat n, CmpNat m n ~ GT, SubUniverse ('At n) (Indexed m universe) ~ universe) =>
         Select (At n) (Indexed m universe) where
  select _ (Indexed fixtures) = fixtures !! (fromIntegral $ natVal (Proxy :: Proxy n))

instance (KnownNat m, KnownNat n, CmpNat m n ~ GT, SubUniverse ('At n) (Mirrored (Indexed m universe)) ~ universe) =>
         Select (At n) (Mirrored (Indexed m universe)) where
  select _ (Mirrored (Indexed fixtures)) = fixtures !! (fromIntegral $ natVal (Proxy :: Proxy m) - 1 - (natVal (Proxy :: Proxy n)))

class SetParam param universe where
  setParam :: Param param -> universe -> [(Address, Value)]

{-
MVector.replicate 30 0 >>= \frame -> (setParam (red 10) (select (Proxy :: Proxy Hex12p1) universe) frame) >> (print =<< Vector.freeze frame)
-}
instance (KnownNat (ParamNat (Proxy param) (Proxy params))) => SetParam param (Fixture params) where
  setParam (Param val) (Fixture address) =
        let n = natVal (Proxy :: Proxy (ParamNat (Proxy param) (Proxy params)))
        in [(address + fromIntegral n, val)]
{-
    in write frame ((fromIntegral address) + (fromIntegral n)) val
-}
instance (SetParam param u) => SetParam param (Labeled lbl u) where
  setParam p (Labeled u) = setParam p u

instance (SetParam param u) => SetParam param (Indexed n u) where
  setParam p (Indexed us) = concatMap (\u -> setParam p u) us

instance (SetParam param u, SetParam param us) => SetParam param (u :+: us) where
  setParam p  (u :+: us) =
    setParam p u ++ setParam p us

-- * SetParams

class SetParams params universe where
  setParams :: params -> universe -> [(Address, Value)]

instance {-# OVERLAPPING #-} (SetParam p1 universe, SetParam p2 universe) => SetParams (Param p1 :+: Param p2) universe where
  setParams (p1 :+: p2) universe =
    (setParam p1 universe) ++ (setParam p2 universe)

instance {-# OVERLAPPING #-} (SetParam p universe, SetParams ps universe) => SetParams (Param p :+: ps) universe where
  setParams (p :+: ps) universe =
    (setParam p universe) ++ (setParams ps universe)

-- * ParamVal

class ToParam p where
  type ToParameter p :: Parameter
  toParam :: p -> Param (ToParameter p)


sp :: (ToParam p, SetParam (ToParameter p) u) => p -> u -> [(Address, Value)]
sp p u = setParam (toParam p) u

