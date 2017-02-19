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
module Main where

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
import Data.Traversable (mapAccumR, mapM)
import Data.Monoid ((<>))
import Data.Word (Word8, Word16)
import Data.Proxy (Proxy(..))
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Data.Vector.Mutable (IOVector, write)
import qualified Data.Vector.Mutable as MVector
import FRP.Netwire.Move (integral)
import GHC.TypeLits -- (TypeError, ErrorMessage(..), Nat, Symbol, KnownNat(..), natVal, natVal')
import GHC.Exts
import Prelude hiding ((.), id, until, mapM)
import System.Environment (getArgs)
import System.Hardware.Serialport as Serialport (CommSpeed(CS9600, CS115200), SerialPort, SerialPortSettings(commSpeed), closeSerial, defaultSerialSettings, openSerial, recv, send, flush)
import System.MIDI as MIDI
import System.MIDI.Utility as MIDI

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

data Waveform
  = Sine
  | Tri
  | Square
  | Ramp
  | PWM Float
    deriving (Eq, Show)

lfo :: Waveform -> Int -> Double -> MidiWire a Double
lfo Sine period phase =
  let i = (2*pi) / (fromIntegral period)
  in proc _ ->
      do t <- time -< () -- time in ticks, 96 ticks per measure
         returnA -< sin ((i * fromIntegral t) + phase)
lfo Ramp period phase = -- FIXME phase
  proc _ ->
    do t <- time -< ()
       returnA -< 1 - (fromIntegral period / (fromIntegral (t `mod` period)))

flame u p =
  let dur = whole * 2
      maxH = 25
      minH = 0
  in
    proc _ ->
      do l <- lfo Sine dur p -< ()
         let a = 15
             o = 15
             v' = (l * a) + o
             h = if v' < 0 then 360 + v' else v'
         returnA -< setParams (hsl $ HSL h 1 0.5) u
--         returnA -< 
{-
  (for (dur + 1) .
     proc midi ->
       do t <- time -< ()
          let p = (fromIntegral t)/(fromIntegral dur) :: Double
          returnA -< (setParams (hsl $ HSL (minH + ((maxH-minH)*p)) 1 0.5) u)) -->
  (for (dur + 1) .
     proc midi ->
       do t <- time -< ()
          let p = (fromIntegral t)/(fromIntegral dur)
          returnA -< (setParams (hsl $ HSL (maxH - ((maxH-minH)*p)) 1 0.5) u)) -->
  flame u
-}
{-
flame u =
  let dur = whole
      maxH = 25
      minH = 0
  in
  (for (dur + 1) .
     proc midi ->
       do t <- time -< ()
          let p = (fromIntegral t)/(fromIntegral dur) :: Double
          returnA -< (setParams (hsl $ HSL (minH + ((maxH-minH)*p)) 1 0.5) u)) -->
  (for (dur + 1) .
     proc midi ->
       do t <- time -< ()
          let p = (fromIntegral t)/(fromIntegral dur)
          returnA -< (setParams (hsl $ HSL (maxH - ((maxH-minH)*p)) 1 0.5) u)) -->
  flame u
-}

masters =
  let par1 = (select gb_par_1 (select gb_1 universe))
      par2 = (select gb_par_2 (select gb_1 universe))
  in proc e ->
      do m1  <- pure $ setParam (master 255) (select slimPar64_1 universe :+: select hex12p1 universe) -< e
         m2  <- pure $ setParam (parCon $ ParConRGB 127) (par1 :+: par2) -< e
         returnA -< mergeParams m1 m2

flames =
  proc e ->
    do -- f <- flame (select slimPar64_1 universe :+: select ultrabar_1 universe :+: select hex12p1 universe :+: select gb_par_1 (select gb_1 universe) :+: select gb_par_2 (select gb_1 universe)) -< e
       f1 <- flame (select slimPar64_1 universe) 0     -< e
       f2 <- flame (select ultrabar_1 universe) (pi/2) -< e
       f3 <- flame (select hex12p1 universe)    (pi)   -< e
       f4 <- flame (select gb_par_1 (select gb_1 universe)) (3*pi/2)   -< e
       f5 <- flame (select gb_par_2 (select gb_1 universe)) (3*pi/2)   -< e
--        :+: select ultrabar_1 universe :+: select hex12p1 universe :+: select gb_par_1 (select gb_1 universe) :+: select gb_par_2 (select gb_1 universe)
       d1  <- pure (setParams ((derbyCon DerbyConRed) :+: (derbyRotation (DerbyClockwise 100))) (select gb_derby_1 (select gb_1 universe))) -< e
       d2  <- pure (setParams ((derbyCon DerbyConRed) :+: (derbyRotation (DerbyCounterClockwise 100))) (select gb_derby_2 (select gb_1 universe))) -< e
       m <- masters -< e
       returnA -< mergeParamsL [m, f1, f2, f3, f4, f5, d1, d2]

pulse :: (Num n) => Int -> MidiWire a n
pulse dur =
 (for (5) . pure 1) --> (for (dur - 3) . pure 0) --> pulse dur

flickerPurple dur =
  do proc e ->
       do m <- masters -< e
          p1 <- lfo Sine dur 0 -< e
          p2 <- lfo Sine dur pi/2 -< e
          p3 <- lfo Sine dur pi -< e
          p4 <- lfo Sine dur 3*pi/2 -< e
          returnA -< concat [ m
                            , setParams (hsl $ HSL 290 1 (0.5+(p1*0.2))) (select hex12p1 universe)
                            , setParams (hsl $ HSL 290 1 (0.5+(p2*0.2))) (select ultrabar_1 universe)
                            , setParams (hsl $ HSL 290 1 (0.5+(p3*0.2))) (select slimPar64_1 universe)
                            , setParams (hsl $ HSL 290 1 (0.5+(p4*0.2))) (select gb_par_1 (select gb_1 universe) :+: select gb_par_2 (select gb_1 universe))
                            ]

pulseWhite dur =
  proc e ->
    do m <- masters -< e
       strb <- gbStrobes1 -< e
       p1 <- pulse dur -< e
       p2 <- (for sixteenth . pure 0) --> pulse dur -< e
       p3 <- (for (2*sixteenth) . pure 0) --> pulse dur -< e
       p4 <- (for (3*sixteenth) . pure 0) --> pulse dur -< e
       h <- arr (\p -> setParam (white (p*255)) (select hex12p1 universe)) -< p1
       u <- arr (\p -> setParams (rgbWhite (p*255)) (select ultrabar_1 universe)) -< p2
       p <- arr (\p -> setParams (rgbWhite (p*255)) ((select gb_par_1 (select gb_1 universe)) :+: (select gb_par_2 (select gb_1 universe)))) -< p3
       s <- arr (\p -> setParams (rgbWhite (p*255)) (select slimPar64_1 universe)) -< p4
       returnA -< mergeParamsL [m, h, u, p, s, strb]

pulseChaos dur =
  proc e ->
    do m <- masters -< e
       p1 <- pulse dur -< e
       p2 <- (for sixteenth . pure 0) --> pulse dur -< e
       p3 <- (for (2*sixteenth) . pure 0) --> pulse dur -< e
       p4 <- (for (3*sixteenth) . pure 0) --> pulse dur -< e
       h <- arr (\p -> setParam (white (p*255)) (select hex12p1 universe)) -< p1
       u <- arr (\p -> setParam (red (p*255)) (select ultrabar_1 universe)) -< p2
       p <- arr (\p -> setParam (blue (p*255)) ((select gb_par_1 (select gb_1 universe)) :+: (select gb_par_2 (select gb_1 universe)))) -< p3
       s <- arr (\p -> setParam (green (p*255)) (select slimPar64_1 universe)) -< p4
       returnA -< mergeParamsL [m, h, u, p, s]

redGreen dur =
  proc e ->
    do m <- masters -< e
       l1 <- lfo Sine dur 0 -< ()
       l2 <- lfo Sine dur pi -< ()
       d1  <- pure (setParams ((derbyCon DerbyConRedGreen) :+: (derbyRotation (DerbyClockwise 100))) (select gb_derby_1 (select gb_1 universe))) -< e
       d2  <- pure (setParams ((derbyCon DerbyConRedGreen) :+: (derbyRotation (DerbyCounterClockwise 100))) (select gb_derby_2 (select gb_1 universe))) -< e
       returnA -< concat [ m, d1, d2
                         , setParams (hsl $ HSL ((l1 + 1)*75) 1 0.5) (select hex12p1 universe)
                         , setParams (hsl $ HSL ((l1 + 1)*75) 1 0.5) (select ultrabar_1 universe)
                         , setParams (hsl $ HSL ((l2 + 1)*75) 1 0.5) (select slimPar64_1 universe)
                         , setParams (hsl $ HSL ((l2 + 1)*75) 1 0.5) (select gb_par_1 (select gb_1 universe) :+: select gb_par_2 (select gb_1 universe))
                         ]
--       v1 <- arr
{-  
   proc e ->
      do m <- masters -< e
         h <- pure $ setParam (white 255) (select hex12p1 universe) -< e
         returnA -< mergeParamsL [m, h]) -->
 (for (dur-3) .
   proc e ->
      do m <- masters -< e
         h <- pure $ setParam (white 0) (select hex12p1 universe) -< e
         returnA -< mergeParamsL [m, h]) -->
 pulse dur
-}
{-
pulse :: Int -> MidiLights
pulse dur =
 (for (5) .
   proc e ->
      do m <- masters -< e
         h <- pure $ setParam (white 255) (select hex12p1 universe) -< e
         returnA -< mergeParamsL [m, h]) -->
 (for (dur-3) .
   proc e ->
      do m <- masters -< e
         h <- pure $ setParam (white 0) (select hex12p1 universe) -< e
         returnA -< mergeParamsL [m, h]) -->
 pulse dur
-}
--       par <-

-- | HSL to RGB
-- h = 0 to 360
-- s = 0 to 1
-- l = 0 to 1
hsl :: HSL Double -> Param 'Red :+: (Param 'Green :+: Param 'Blue)
hsl v =
  case rgb_d2w (hsl2rgb v) of
    (RGB r g b) -> red r :+: green g :+: blue b

uv :: Value -> Param UV
uv = Param

master :: Value -> Param Master
master v = Param v

data GBParConVal
  = ParConRGB Word8 -- ^ 0 to 127
  | ParConStrobeSpeed Word8 -- ^ 0 to 111
  | ParConStrobeSound Word -- ^ 0 to 9
  | ParConRGB100 Word8 -- ^ 0 to 5
    deriving Show

parCon :: GBParConVal -> Param GBParCon
parCon cv =
  case cv of
    (ParConRGB v) -> Param (min v 127)

data GBDerbyConVal
  = DerbyConBlackout
  | DerbyConRed
  | DerbyConGreen
  | DerbyConBlue
  | DerbyConRedGreen
  | DerbyConRedBlue
  | DerbyConGreenBlue
  | DerbyConRedGreenBlue
  | DerbyConAuto1
  | DerbyConAuto2
    deriving Show

derbyCon :: GBDerbyConVal -> Param GBDerbyCon
derbyCon v =
  case v of
    (DerbyConBlackout)     -> Param $ 0
    (DerbyConRed)          -> Param $ 25
    (DerbyConGreen)        -> Param $ 50
    (DerbyConBlue)         -> Param $ 75
    (DerbyConRedGreen)     -> Param $ 100
    (DerbyConRedBlue)      -> Param $ 125
    (DerbyConGreenBlue)    -> Param $ 125
    (DerbyConRedGreenBlue) -> Param $ 175
    (DerbyConAuto1)        -> Param $ 200
    (DerbyConAuto2)        -> Param $ 225

data GBDerbyStrobeVal
  = DerbyStrobeOff
  | DerbyStrobeRate Word8 -- ^ 0 to 229
  | DerbyStrobeSound
    deriving Show

derbyStrobe :: GBDerbyStrobeVal -> Param GBDerbyStrobe
derbyStrobe v =
  case v of
    DerbyStrobeOff    -> Param 0
    DerbyStrobeRate n -> Param $ 10 + min n 229
    DerbyStrobeSound  -> Param $ 240

data GBDerbyRotationVal
  = DerbyStop
  | DerbyClockwise Word8 -- ^ 0 to 122
  | DerbyCounterClockwise Word8 -- ^ 0 to 121
    deriving Show

derbyRotation :: GBDerbyRotationVal -> Param GBDerbyRotation
derbyRotation v =
  case v of
    DerbyStop      -> Param 0
    DerbyClockwise n -> Param $ 5 + min n 122
    DerbyCounterClockwise n -> Param $ 134 + min n 121

data GBLaserColorVal
  = GBLaserBlackout
  | GBLaserRed
  | GBLaserGreen
  | GBLaserRedGreen
  | GBLaserRedGreenStrobe
  | GBLaserRedStrobeGreen
  | GBLaserRedGreenAlternate
    deriving (Eq, Ord, Read, Show)

gbLaserColor :: GBLaserColorVal -> Param GBLaserColor
gbLaserColor v =
  case v of
    GBLaserBlackout -> Param  0
    GBLaserRed      -> Param 40
    GBLaserGreen    -> Param 80
    GBLaserRedGreen    -> Param 120
    GBLaserRedGreenStrobe -> Param 160
    GBLaserRedStrobeGreen -> Param 200
    GBLaserRedGreenAlternate -> Param 240

data GBLaserStrobeVal
  = GBLaserStrobeOff
  | GBLaserStrobeRate Word8 -- ^ 0 to 229
  | GBLaserStrobeSound
    deriving (Eq, Ord, Read, Show)

gbLaserStrobe :: GBLaserStrobeVal -> Param GBLaserStrobe
gbLaserStrobe v =
  case v of
    GBLaserStrobeOff -> Param 0
    GBLaserStrobeRate n -> Param $ 10 + min n 229
    GBLaserStrobeSound -> Param 240

data GBLaserPatternVal
  = GBLaserStop
  | GBLaserClockwise Word8 -- ^ 0 to 122
  | GBLaserCounterClockwise Word8 -- ^ 0 to 121
    deriving (Eq, Ord, Read, Show)

gbLaserPattern :: GBLaserPatternVal -> Param GBLaserPattern
gbLaserPattern v =
  case v of
    GBLaserStop                 -> Param $ 0
    (GBLaserClockwise n)        -> Param $ 5 + min n 122
    (GBLaserCounterClockwise n) -> Param $ 134 + min n 122

data GBStrobePatternVal
  = GBStrobeBlackout
  | GBStrobeAuto1
  | GBStrobeAuto2
  | GBStrobeAuto3
  | GBStrobeAuto4
  | GBStrobeAuto5
  | GBStrobeAuto6
  | GBStrobeAuto7
  | GBStrobeAuto8
  | GBStrobeAuto9
  | GBStrobeSpeed'
  | GBStrobeSound
    deriving (Eq, Ord, Read, Show)

gbStrobePattern :: GBStrobePatternVal
                   -> Param GBStrobePattern
gbStrobePattern v =
  case v of
    GBStrobeBlackout -> Param 0
    GBStrobeAuto1    -> Param 10
    GBStrobeAuto2    -> Param 30
    GBStrobeAuto3    -> Param 50
    GBStrobeAuto4    -> Param 70
    GBStrobeAuto5    -> Param 90
    GBStrobeAuto6    -> Param 110
    GBStrobeAuto7    -> Param 130
    GBStrobeAuto8    -> Param 150
    GBStrobeAuto9    -> Param 170
    GBStrobeSpeed'    -> Param 190
    GBStrobeSound    -> Param 210

gbStrobeSpeed :: Word8 -> Param GBStrobeSpeed
gbStrobeSpeed v = Param v

gbStrobeDimmer :: Word8 -> Param GBStrobeDimmer
gbStrobeDimmer v = Param v

data Labeled (name :: Symbol) a = Labeled a

data Indexed (i :: Nat) a = Indexed [a]
                            deriving Show

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

modeMap :: Map Int MidiLights
modeMap = Map.fromList
  [ (0, redBlue)
  , (1, strobe)
  , (2, gbDerbys)
  , (3, allRedBlue)
  , (4, gbStrobes1)
  , (5, gbStrobes1')
  , (6, flames)
  , (7, pulseChaos quarter)
  , (8, pulseWhite quarter)
  , (9, redGreen whole)
  , (10, flickerPurple quarter)
  ]


midiModes :: Map Int MidiLights -> MidiLights
midiModes midiModes = loop (Map.fromList []) -- [(0,redBlue), (1, strobe)])
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

midi2seq :: MidiWire (Midi2Seq, Event Input) Output
midi2seq =
  proc (m2s, e) ->
    do -- app -< (foldLights (Map.elems m2s),e)
       returnA -<  undefined
--   do o <- (app $ arr (\(m2s, e) -> (foldLights (Map.elems m2s), e))) -< (m2s, e)
--   do o <- (arr (\_ -> undefined) >>> app) -< (m2s, e)

multi :: MidiLights
multi =
   proc e ->
    do rb <- redBlue -< e
       p  <- strobe  -< e
       returnA -< mergeParams rb p

allRedBlue =
  proc e ->
     do rb <- redBlueU (select slimPar64_1 universe :+: select ultrabar_1 universe :+: select gb_par_1 (select gb_1 universe) :+: select gb_par_2 (select gb_1 universe)) -< e
        m  <- pure $ setParam (master 255) (select slimPar64_1 universe :+: select hex12p1 universe) -< e
        parsM <- pure $ sp (ParConRGB 127) (let u = select gb_1 universe in select gb_par_1 u :+: select gb_par_2 u) -< e
--        s <- strobe -< e
        returnA -< mergeParamsL [rb,m, parsM]

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


sixteenth, eighth, quarter, whole :: (Num a) => a
sixteenth  = 6
eighth     = 12
quarter    = 24
half       = 48
whole      = 96

beatSession :: (MonadIO m) => MidiSession m
beatSession =
  Session $ do
--    p0 <- liftIO $ atomically $ waitN pos 1
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

type family ParamNat param params where
  ParamNat (Proxy a) (Proxy (a ': bs)) = 0
  ParamNat (Proxy a) (Proxy (b ': bs)) = 1 + ParamNat (Proxy a) (Proxy bs)

type family SubUniverse (path :: Path) universe :: * where
  SubUniverse (Label lbl) (Labeled lbl universe) = universe
  SubUniverse (Label lbl) (Labeled lbl universe :+: universes) = universe
  SubUniverse (Label lbl) (universe :+: universes) = SubUniverse (Label lbl) universes
  SubUniverse (At n) (Indexed m universe) = universe
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

instance ToParam GBParConVal where
  type ToParameter GBParConVal = GBParCon
  toParam = parCon

instance ToParam GBLaserColorVal where
  type ToParameter GBLaserColorVal = GBLaserColor
  toParam = gbLaserColor

sp :: (ToParam p, SetParam (ToParameter p) u) => p -> u -> [(Address, Value)]
sp p u = setParam (toParam p) u

-- * sequences

blackout :: MidiLights
blackout = arr (const [])

redBlue :: MidiLights
redBlue =
  let dur = quarter + 1
      u = (select hex12p1 universe)
  in
  for dur . pure (concat [setParam (red 255) u, setParam (master 255) u]) -->
  for dur . pure (concat [setParam (blue 255) u, setParam (master 255) u]) -->
  redBlue

redBlueU :: (SetParam 'Red universe, SetParam 'Blue universe) => universe -> MidiLights
redBlueU u =
  let dur = quarter + 1 in
  for dur . pure (setParam (red 255) u) -->
  for dur . pure (setParam (blue 255) u) -->
  redBlueU u


-- pulsar :: MidiLights
pulsar u =
  (for (quarter + 1) .
     proc midi ->
       do t <- time -< ()
          returnA -< (setParam (red (fromIntegral (t * 10))) u)) -->
  (for (quarter + 1) .
     proc midi ->
       do t <- time -< ()
          returnA -< (setParam (red (fromIntegral (255 - (t*10)))) u)) -->
  pulsar u

strobe :: MidiLights
strobe  =
  let onTime = 1 in
  proc _ ->
    do t <- fmap (`mod` eighth) time -< ()
       returnA -< if t == 0
                  then setParam (white 255) (select hex12p1 universe)
                  else []

-- * SlimPar64

type SlimPar64_7channel = '[Red, Green, Blue, SP64ColorMacros, SP64SpeedStrobe, S64Mode, Master]

slimPar64 :: Address -> Fixture SlimPar64_7channel
slimPar64 = Fixture

type Def_SlimPar64_1 = Labeled "slimPar64_1" (Fixture SlimPar64_7channel)

def_slimPar64_1 :: Def_SlimPar64_1
def_slimPar64_1 = Labeled $ slimPar64 1

type SlimPar64_1 = Label "slimPar64_1"

slimPar64_1 :: Proxy SlimPar64_1
slimPar64_1 = Proxy

-- * Hex12p

type Hex12p_7channel = '[Red, Green, Blue, White, Amber, UV, Master]

hex12p :: Address -> Fixture Hex12p_7channel
hex12p addr = Fixture { address = addr }

def_hex12p1 :: Labeled "hex12p1" (Fixture Hex12p_7channel)
def_hex12p1 = Labeled $ hex12p 8

type Hex12p1 = Label "hex12p1"

hex12p1 :: Proxy Hex12p1
hex12p1 = Proxy

-- * Gigbar

type Par   = [Red, Green, Blue, GBParCon]
type Derby = [GBDerbyCon, GBDerbyStrobe, GBDerbyRotation]
type Laser = [GBLaserColor, GBLaserStrobe, GBLaserPattern]
type Strobe = [GBStrobePattern, GBStrobeDimmer, GBStrobeSpeed]

type Def_GB_Par_1 = Labeled "gb_par_1" (Fixture Par)

def_gb_par_1 :: Address -> Def_GB_Par_1
def_gb_par_1 = Labeled . Fixture

type GB_Par_1 = Label "gb_par_1"

gb_par_1 :: Proxy GB_Par_1
gb_par_1 = Proxy

type Def_GB_Par_2 = Labeled "gb_par_2" (Fixture Par)

def_gb_par_2 :: Address -> Def_GB_Par_2
def_gb_par_2 = Labeled . Fixture

type GB_Par_2 = Label "gb_par_2"

gb_par_2 :: Proxy GB_Par_2
gb_par_2 = Proxy

type Def_GB_Derby_1 = Labeled "gb_derby_1" (Fixture Derby)

def_gb_derby_1 :: Address -> Def_GB_Derby_1
def_gb_derby_1 = Labeled . Fixture

type GB_Derby_1 = Label "gb_derby_1"

gb_derby_1 :: Proxy GB_Derby_1
gb_derby_1 = Proxy

type Def_GB_Derby_2 = Labeled "gb_derby_2" (Fixture Derby)

def_gb_derby_2 :: Address -> Def_GB_Derby_2
def_gb_derby_2 = Labeled . Fixture

type GB_Derby_2 = Label "gb_derby_2"

gb_derby_2 :: Proxy GB_Derby_2
gb_derby_2 = Proxy

type Def_GB_Laser = Labeled "gb_laser" (Fixture Laser)

def_gb_laser :: Address -> Def_GB_Laser
def_gb_laser = Labeled . Fixture

type GB_Laser = Label "gb_laser"

gb_laser :: Proxy GB_Laser
gb_laser = Proxy

type Def_GB_Strobe = Labeled "gb_strobe" (Fixture Strobe)

def_gb_strobe :: Address -> Def_GB_Strobe
def_gb_strobe = Labeled . Fixture

type GB_Strobe = Label "gb_strobe"

gb_strobe :: Proxy GB_Strobe
gb_strobe = Proxy

type GigBar = (Def_GB_Par_1 :+: Def_GB_Par_2 :+: Def_GB_Derby_1 :+: Def_GB_Derby_2 :+: Def_GB_Laser :+: Def_GB_Strobe)

type Def_GigBar_1 = Labeled "gigbar_1" GigBar

def_gb :: Address -> GigBar
def_gb addr =
  def_gb_par_1 addr :+:
  def_gb_par_2 (addr + 4) :+:
  def_gb_derby_1 (addr + 8) :+:
  def_gb_derby_2 (addr + 11) :+:
  def_gb_laser (addr + 14) :+:
  def_gb_strobe (addr + 17)

def_gb_1 :: Def_GigBar_1
def_gb_1 = Labeled $ def_gb (7+7+18+1)

type GigBar_1 = Label "gigbar_1"

gb_1 :: Proxy GigBar_1
gb_1 = Proxy

-- * Ultrabar

type Ultrabar_RGB = [Red, Green, Blue]

type Ultrabar_18 = Indexed 6 (Fixture Ultrabar_RGB)

ultrabar :: Address -> Ultrabar_18
ultrabar addr =
  Indexed $
   [ Fixture { address = addr      }
   , Fixture { address = addr +  3 }
   , Fixture { address = addr +  6 }
   , Fixture { address = addr +  9 }
   , Fixture { address = addr + 12 }
   , Fixture { address = addr + 15 }
   ]

type Def_Ultrabar_1 = Labeled "ultrabar_1" Ultrabar_18

def_ultrabar_1 :: Def_Ultrabar_1 -- Labeled "ultrabar_1" Ultrabar_18
def_ultrabar_1 = Labeled $ ultrabar 15

type Ultrabar_1 = Label "ultrabar_1"

ultrabar_1 :: Proxy Ultrabar_1
ultrabar_1 = Proxy

type Universe =
  Labeled "hex12p1" (Fixture Hex12p_7channel) :+:
  Def_SlimPar64_1 :+:
  Def_Ultrabar_1 :+:
  Def_GigBar_1

universe :: Universe
universe = def_hex12p1 :+: def_slimPar64_1 :+: def_ultrabar_1 :+: def_gb_1

gbRedBlue =
  let par1 = (select gb_par_1 (select gb_1 universe))
      par2 = (select gb_par_2 (select gb_1 universe))
  in proc e ->
      do rb1 <- redBlueU par1  -< e
         rb2 <- redBlueU par2  -< e
         m1  <- pure $ setParam (parCon $ ParConRGB 127) par1 -< e
         m2  <- pure $ setParam (parCon $ ParConRGB 127) par2 -< e
         returnA -< mergeParamsL [m1, m2, rb1, rb2]

-- gbDerbys' :: MidiLights
gbDerbys' db1 =
  let -- db1 = (select gb_derby_1 (select gb_1 universe))
      dur = quarter + 1
  in -- for dur . arr (\_ -> setParam (derbyCon DerbyConRed) db1 ++ setParam (derbyRotation (DerbyClockwise 3)) db1) -->
     for dur . pure (setParam (derbyCon DerbyConRed) db1) -->
     for dur . pure (setParam (derbyCon DerbyConGreen) db1) -->
     for dur . pure (setParam (derbyCon DerbyConBlue) db1) -->
     for dur . pure (setParam (derbyCon DerbyConRedBlue) db1) -->
     for dur . pure (setParam (derbyCon DerbyConRedGreen) db1) -->
     for dur . pure (setParam (derbyCon DerbyConGreenBlue) db1) -->
     for dur . pure (setParam (derbyCon DerbyConRedGreenBlue) db1) -->
     gbDerbys' db1

-- gbDerbyRot :: MidiLights
gbDerbyRot db1 =
  let -- db1 = (select gb_derby_1 (select gb_1 universe))
      dur = whole + 1
  in for dur . pure (setParam (derbyRotation (DerbyClockwise 100)) db1) -->
     for dur . pure (setParam (derbyRotation (DerbyCounterClockwise 100)) db1) -->
     gbDerbyRot db1

gbDerbyRot' db1 =
  let -- db1 = (select gb_derby_1 (select gb_1 universe))
      dur = whole + 1
  in for dur . pure (setParam (derbyRotation (DerbyCounterClockwise 100)) db1) -->
     for dur . pure (setParam (derbyRotation (DerbyClockwise 100)) db1) -->
     gbDerbyRot' db1

gbDerbyStrobe db =
  pure (setParam (derbyStrobe (DerbyStrobeRate 228)) db)

-- gbDerbys :: MidiLights
gbDerby db1 =
  let a = 1
      -- db1 = (select gb_derby_1 (select gb_1 universe))
  in proc e ->
       do colors <- gbDerbys' db1 -< e
          -- colors <- pure (setParam (derbyCon DerbyConRed) db1) -< e
          rot <- gbDerbyRot db1 -< e
--          strobe <- gbDerbyStrobe db1 -< e
          returnA -< mergeParamsL [colors, rot]

gbDerby' db1 =
  let a = 1
      -- db1 = (select gb_derby_1 (select gb_1 universe))
  in proc e ->
       do colors <- gbDerbys' db1 -< e
          rot <- gbDerbyRot' db1 -< e
  --        strobe <- gbDerbyStrobe db1 -< e
          returnA -< mergeParamsL [colors, rot]

gbDerbys :: MidiLights
gbDerbys =
  proc e ->
    do db1 <- gbDerby (select gb_derby_1 (select gb_1 universe)) -< e
       db2 <- gbDerby' (select gb_derby_2 (select gb_1 universe)) -< e
       returnA -< mergeParamsL [db1, db2]

-- gbDerbysOff :: MidiLights
gbDerbyOff db1 =
    let a = 1
        -- db1 = (select gb_derby_1 (select gb_1 universe))
    in pure (concat [ setParam (derbyCon DerbyConBlackout) db1
                    , setParam (derbyRotation DerbyStop) db1
                    ])

gbDerbysOff :: MidiLights
gbDerbysOff = (gbDerbyOff (select gb_derby_1 (select gb_1 universe)) &&&
              gbDerbyOff (select gb_derby_1 (select gb_1 universe))) >>> arr (uncurry mergeParams)
{-
  proc e ->
    do d1 <- pure $ setParam  (derbyCon $ DerbyConRed)  -< e
       returnA -< mergeParamsL [d1]
-}


-- gbLaserColors :: MidiLights
gbLaserColors lzr =
    let dur = whole + 1
  in for dur . pure (sp GBLaserRed lzr) -->
     for dur . pure (sp GBLaserGreen lzr) -->
     for dur . pure (sp GBLaserRedGreen lzr) -->
     for dur . pure (sp GBLaserRedGreenStrobe lzr) -->
     for dur . pure (sp GBLaserRedStrobeGreen lzr) -->
     for dur . pure (sp GBLaserRedGreenAlternate lzr) -->
     for dur . pure (sp GBLaserBlackout lzr) -->
     gbLaserColors lzr

laserStrobe lzr =
  pure $ setParam (gbLaserStrobe (GBLaserStrobeRate 100)) lzr

laserPattern lzr =
  let dur = whole + 1
  in for dur . pure (setParam (gbLaserPattern (GBLaserCounterClockwise 100)) lzr) -->
     for dur . pure (setParam (gbLaserPattern (GBLaserClockwise 100)) lzr) -->
     laserPattern lzr

laserOff lzr =
  pure $ concat [ setParam (gbLaserPattern GBLaserStop) lzr
                , setParam (gbLaserColor GBLaserBlackout) lzr
                ]

-- gbLasers :: MidiLights
gbLasers lzr = gbLaserColors lzr

gbLasers1 :: MidiLights
gbLasers1 =
  let lzr = select gb_laser (select gb_1 universe)
  in proc e ->
       do c <- gbLasers lzr -< e
          s <- laserStrobe lzr -< e
          p <- laserPattern lzr -< e
          returnA -< mergeParamsL [c,s,p]

strobePattern lzr =
  pure $ setParam (gbStrobePattern GBStrobeAuto9) lzr

strobeDimmer lzr =
  pure $ setParam (gbStrobeDimmer 255) lzr

strobeSpeed lzr =
  pure $ setParam (gbStrobeSpeed 222) lzr

gbStrobes1 :: MidiLights
gbStrobes1 =
    let strb = select gb_strobe (select gb_1 universe)
    in proc e ->
         do s <- strobeSpeed strb -< e
            p <- strobePattern strb -< e
            d <- strobeDimmer strb -< e
            returnA -< mergeParamsL [p,d,s]

gbStrobes1' :: MidiLights
gbStrobes1' =
    let strb = select gb_strobe (select gb_1 universe)
    in proc e ->
         do s <- strobeSpeed strb -< e
            p <- strobePattern strb -< e
            returnA -< mergeParamsL [p,s]

gbBlackout gb =
  let par1   = select gb_par_1 gb
      par2   = select gb_par_2 gb
      derby1 = select gb_derby_1 gb
      derby2 = select gb_derby_2 gb
      laser  = select gb_laser gb
      strobe = select gb_strobe gb
  in proc e ->
      do p1 <- pure $ setParam (parCon $ ParConRGB 0) par1 -< e
         p2 <- pure $ setParam (parCon $ ParConRGB 0) par2 -< e
         d1 <- gbDerbyOff derby1 -< e
         d2 <- gbDerbyOff derby2 -< e
         l  <- laserOff laser -< e
         s  <- pure $ setParam (gbStrobePattern GBStrobeBlackout) strobe -< e
         returnA -< mergeParamsL [p1,p2, d1, d2]

pulsarPar1 :: MidiLights
pulsarPar1 =
  let p1 = (select gb_par_1 (select gb_1 universe)) in
  proc e ->
    do p <- pulsar p1 -< e
       m <- pure $ sp (ParConRGB 100) p1 -< e
       returnA -< mergeParamsL [p,m]

main :: IO ()
main =
  do queue <- atomically newTQueue
     bracket (createDestination "DMX" (Just $ callback queue)) (\c -> putStrLn "disposing of connection." >> disposeConnection c) $ \midiIn ->
       bracket_ (start midiIn) (putStrLn "closing midi" >> MIDI.close midiIn) $
        withSerial dmxPort dmxBaud $ \s ->
         do runShow queue (serialOutput s) beatSession (midiModes modeMap)
