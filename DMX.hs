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
module Main where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TMVar (TMVar, newEmptyTMVar, putTMVar, takeTMVar, tryTakeTMVar)
import Control.Concurrent.STM.TQueue (TQueue, newTQueue, writeTQueue, readTQueue)
import Control.Exception (bracket, bracket_)
import Control.Monad.Trans (MonadIO(..), lift)
import Control.Wire hiding ((<>))
import Control.Wire.Core
import Control.Wire.Unsafe.Event
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Monoid ((<>))
import Data.Word (Word16)
import Data.Proxy (Proxy(..))
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Data.Vector.Mutable (IOVector, write)
import qualified Data.Vector.Mutable as MVector
import FRP.Netwire.Move (integral)
import GHC.TypeLits -- (TypeError, ErrorMessage(..), Nat, Symbol, KnownNat(..), natVal, natVal')
import GHC.Exts
import Prelude hiding ((.), id, until)
import System.Environment (getArgs)
import System.Hardware.Serialport (CommSpeed(CS9600, CS115200), SerialPort, SerialPortSettings(commSpeed), defaultSerialSettings, openSerial, send)
import System.MIDI as MIDI
import System.MIDI.Utility as MIDI

-- | FIXME: handle longer than 254
-- FIXME: handle 0xFF and non-zero termination
stuff :: ByteString -> ByteString
stuff b | B.length b > 254 = error "stuff not implemented for ByteStrings longer than 254 bytes. Submit a patch!"
stuff b = (B.cons 0 (stuff' (B.snoc b 0)))
  where
    stuff' :: ByteString -> ByteString
    stuff' b | B.null b = b
    stuff' b =
      case B.span (/= 0) b of
        (b0, b1) ->
          (B.cons (fromIntegral $ B.length b0 + 1) b0) <> (stuff' $ B.drop 1 b1)

data Parameter
  = Red
  | Green
  | Blue
  | Amber
  | White
  | UV
  | Master
  deriving (Eq, Ord, Read, Show)

red :: Word9 -> Param Red
red v = Param v

green :: Word9 -> Param Green
green v = Param v

blue :: Word9 -> Param Blue
blue v = Param v

type Word9 = Word16
type Address = Word9

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

type Frame = IOVector Word9

data Param (a :: Parameter) = Param Word9

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
dmxPort = "/dev/ttyACM0"

nowNow :: MidiLights
nowNow =
  periodic 1 . (pure [Print "now"])
{-
redBlue :: MidiLights
--redBlue = for quarter . now . (pure (MVector.replicate 30 0) >>= \v -> pure [F v]) -->
redBlue = for quarter . now . (arr (\(Event a) -> pure (Print "hello"))) -->
  redBlue
-}
{-  
  for quarter . now . MVector.replicate 30 0 -->
  for quarter . now . MVector.replicate 30 1 -->
  redBlue
-}
main :: IO ()
main =
  do -- sources <- enumerateSources
     -- print =<< (mapM getName sources)
--     destinations <- enumerateDestinations
--     print =<< (mapM getName destinations)
--     [numStr] <- getArgs
--     let num = read numStr
--         userPortOut = sources!!num
--         userPortIn  = destinations!!num
     queue <- atomically newTQueue
     bracket (createDestination "DMX" (Just $ callback queue)) disposeConnection $ \midiIn ->
--     bracket (openSource userPortOut (Just $ callback queue)) MIDI.close $ \midiIn ->
       bracket_ (start midiIn) (MIDI.close midiIn) $
        do runShow queue printOutput beatSession nowNow
           pure ()

data Action
    = Tick
    | ME MidiEvent

data OutputEvent
  = Print String
  | F ()

type MidiTimed   = Timed Int ()
type MidiSession m = Session m MidiTimed
type MidiWire    = Wire MidiTimed () Identity
type MidiLights  = Wire MidiTimed () Identity (Event Action) (Event [OutputEvent])

sixteenth, eighth, quarter, whole :: Int
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
-- printOutput (F frame) = liftIO $ print . Vector.toList =<< Vector.freeze  frame

runShow :: (MonadIO m) =>
           TQueue Action
        -> (OutputEvent -> m ())
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
        (Right (Event actions)) ->
          do mapM_ output actions
        _                 -> return ()
      loop s w

callback :: TQueue Action -> MidiEvent -> IO ()
callback queue midiEvent =
  do -- print midiEvent
     atomically $ writeTQueue queue (ME midiEvent)
{-
  do v <- MVector.new 10
     MVector.set v 0
--     setParam (Param 12 :: Param 'Green) ultrabar1 v
--     s <- openSerial dmxPort defaultSerialSettings { commSpeed = dmxBaud }
     v' <- Vector.freeze v
     print (Vector.toList v')
-}
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

instance Select (Label lbl) (Labeled lbl universe :+: universes) where
  select _ (Labeled u :+: universes) = u

instance ( HasLabel lbl universe ~ False
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
  setParam :: Param param -> universe -> Frame -> IO ()

{-
MVector.replicate 30 0 >>= \frame -> (setParam (red 10) (select (Proxy :: Proxy Hex12p1) universe) frame) >> (print =<< Vector.freeze frame)
-}
instance (KnownNat (ParamNat (Proxy param) (Proxy params))) => SetParam param (Fixture params) where
  setParam (Param val) (Fixture address) frame =
    let n = natVal (Proxy :: Proxy (ParamNat (Proxy param) (Proxy params)))
    in write frame ((fromIntegral address) + (fromIntegral n)) val

instance (SetParam param u) => SetParam param (Labeled lbl u) where
  setParam p (Labeled u) frame = setParam p u frame

instance (SetParam param u) => SetParam param (Indexed n u) where
  setParam p (Indexed us) frame = mapM_ (\u -> setParam p u frame) us

instance (SetParam param u, SetParam param us) => SetParam param (u :+: us) where
  setParam p  (u :+: us) frame =
    do setParam p u frame
       setParam p us frame

{-
setParam :: ( Select path universe
            ) => Param param -> Proxy path -> universe -> IO ()
setParam (Param val) _ universe = pure ()
-}
-- instance (Select (Label lbl) universes) => Select (Label lbl) (universe :+: universes) where
--  select lbl (u :+: us) =  select (Proxy :: Proxy (Label lbl)) us

  -- undefined -- select lbl us -- undefined -- 1 :: Int -- select lbl us
    -- undefined :: Int -- SubUniverse (Label lbl) (universe :+: universes)
    -- select undefined us -- (select lbl us) :: SubUniverse ('Label lbl) (universes)

{-
class Select (path :: Path) (universe :: *) where
  type SubUniverse path universe
  select :: Proxy path -> universe -> (SubUniverse path universe)

instance Select Here universe where
  type SubUniverse Here universe = universe
  select _ u = u

instance Select (Label lbl) (Labeled lbl universe) where
  type SubUniverse (Label lbl) (Labeled lbl universe) = universe
  select _ (Labeled u) = u

instance Select (Label lbl) (Labeled lbl universe :+: universes) where
  type SubUniverse (Label lbl) (Labeled lbl universe :+: universes) = universe
  select _ (Labeled u :+: universes) = u

instance (Select (Label lbl) universes) => Select (Label lbl) (Labeled lbl' universe :+: universes) where
  type SubUniverse (Label lbl) (Labeled lbl' universe :+: universes) = SubUniverse (Label lbl) universes
  select lbl (u :+: us) = select lbl us

instance (KnownNat n, CmpNat m n ~ GT) => Select (At n) (Indexed m universe) where
  type SubUniverse (At n) (Indexed m universe) = universe
  select at (Indexed us) = us !! (fromIntegral $ natVal (Proxy :: Proxy n))

instance (KnownNat n, CmpNat m n ~ GT) => Select (At n) (Labeled lbl (Indexed m universe)) where
  type SubUniverse (At n) (Labeled lbl (Indexed m universe)) = universe
  select at (Labeled (Indexed us)) = us !! (fromIntegral $ natVal (Proxy :: Proxy n))
-}
{-
instance Select (AT 0) (universe :+: universes) where
  type SubUniverse (AT 0) (universe :+: universes) = universe
  select _ (u :+: us) = u
-}
{- This works until we try to have a list of paths. We need a way to select sub universes.
class SetParam param path universe where
  setParam :: Param param -> path -> universe -> Frame -> IO ()

{-
MVector.replicate 30 0 >>= \frame -> (setParam (green 10) here hex12p1 frame) >> (setParam (red 10) here hex12p1 frame) >> (print =<< Vector.freeze frame)
-}
instance ( Member param params ~ True
         , KnownNat (ParamNat (Proxy param) (Proxy params))
         ) =>
         SetParam param (PATH Here) (Fixture params) where
  setParam (Param val) _ (Fixture address) frame =
    let n = natVal (Proxy :: Proxy (ParamNat (Proxy param) (Proxy params)))
    in write frame ((fromIntegral address) + (fromIntegral n)) val
{-
MVector.replicate 30 0 >>= \frame -> (setParam (green 10) here hex12p1 frame) >> (setParam (red 10) here hex12p1 frame) >> (print =<< Vector.freeze frame)
-}
instance (SetParam param (PATH Here) fixture) =>
         SetParam param (PATH (Labeled lbl)) (Label lbl fixture) where
  setParam p _ (Label fixture) frame = setParam p (PATH :: PATH Here) fixture frame

{-
MVector.replicate 30 0 >>= \frame -> (setParam (green 10) (PATH :: PATH (At 0)) (ultrabar 9) frame) >> (print =<< Vector.freeze frame)
-}
instance (SetParam param (PATH Here) fixtureA) =>
         SetParam param (PATH (At 0)) (fixtureA :+: rest) where
  setParam p _ (fixtureA :+: _) frame = setParam p (PATH :: PATH Here) fixtureA frame

instance (SetParam param (PATH Here) fixtureA) =>
         SetParam param (PATH (At 0)) (fixtureA) where
  setParam p _ (fixtureA) frame = setParam p (PATH :: PATH Here) fixtureA frame

{-
MVector.replicate 30 0 >>= \frame -> (setParam (green 10) (PATH :: PATH (At 2)) (ultrabar 9) frame) >> (print =<< Vector.freeze frame)
-}
instance (CmpNat n 0 ~ GT, SetParam param (PATH (At (n - 1))) fixtures) =>
         SetParam param (PATH (At n)) (fixture :+: fixtures) where
  setParam p _ (_ :+: fixtures) frame = setParam p (PATH :: PATH (At (n - 1))) fixtures frame

instance (SetParam param (PATH (path :+: paths)) universe) where
setParam p _ universe frame = setParam p (PATH ::
-}

-- * Hex12p

type Hex12p_7channel = '[Red, Green, Blue, Amber, White, UV, Master]

hex12p :: Address -> Fixture Hex12p_7channel
hex12p addr = Fixture { address = addr }

hex12p1 :: Labeled "hex12p1" (Fixture Hex12p_7channel)
hex12p1 = Labeled $ hex12p 1

type Hex12p1 = Label "hex12p1"

-- * Ultrabar

type Ultrabar_RGB = [Red, Green, Blue]

type Ultrabar_18 = Indexed 6 (Fixture Ultrabar_RGB)

-- :+: Fixture Ultrabar_RGB :+: Fixture Ultrabar_RGB :+: Fixture Ultrabar_RGB :+: Fixture Ultrabar_RGB :+: Fixture Ultrabar_RGB
{-
ultrabar :: Address -> Ultrabar_18
ultrabar addr =
  Fixture { address = addr      } :+:
  Fixture { address = addr +  3 } :+:
  Fixture { address = addr +  6 } :+:
  Fixture { address = addr +  9 } :+:
  Fixture { address = addr + 12 } :+:
  Fixture { address = addr + 15 }
-}

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

ultrabar1 :: Labeled "ultrabar1" Ultrabar_18
ultrabar1 = Labeled $ ultrabar 9

type Ultrabar1 = Label "ultrabar1"

type Universe = Labeled "hex12p1" (Fixture Hex12p_7channel) :+:
            Labeled "ultrabar1" Ultrabar_18
universe :: Universe
universe = hex12p1 :+: ultrabar1

{-
instance SetParam a (a ': as) where
  setParam (Param v) (Fixture address) frame = frame

instance (Member a bs ~ True) => SetParam a (b ': bs) where
  setParam p@(Param v) f@(Fixture address) frame = setParam p ((Fixture (succ address)) :: Fixture bs) frame
-}


{-
class ParamIndex (a :: Parameter) (l :: [Parameter]) where
  paramIndex :: Param a -> Fixture l -> Int

instance (Member a as ~ False) => ParamIndex a (a ': as) where
  paramIndex _ _ = 0

instance forall a b bs. (Member a bs ~ True, ParamIndex a bs) => ParamIndex a (b ': bs) where
  paramIndex a (Fixture address) = succ (paramIndex a ((Fixture address) :: Fixture bs))
-}

-- type family SetParam (Param p) (Fixture params) where
--  setParam
{-
class SetParam p params where
  setParam :: Param p -> Fixture params -> Frame -> Frame

instance SetParam a (a ': as) where
  setParam (Param v) (Fixture address) frame = frame

instance (Member a bs ~ True) => SetParam a (b ': bs) where
  setParam p@(Param v) f@(Fixture address) frame = setParam p ((Fixture (succ address)) :: Fixture bs) frame
-}

{-
setParam :: (ParamIndex p params) =>
            Param p
         -> Fixture params
         -> Frame
         -> IO ()
setParam p@(Param v) fixture frame =
  let index = paramIndex p fixture
  in write frame index v
-}
{-
type family HasParam param params :: Constraint where
  HasParam p '[] = TypeError (Text "Parameter missing")
  HasParam a (b ': bs) = (a ~ b)
  HasParam a (b ': bs) = (HasParam a bs)

class HasParam param fixture
instance (a ~ b) => HasParam (Param a) (Fixture (b ': bs))
instance (HasParam (Param a) (Fixture bs)) => HasParam (Param a) (Fixture (b ': bs))
-}
-- instance (HasParam (Proxy a) (Fixture '[]) -- throw type error
{-
-- FIXME: does not handle non-unique params
-- throw type error if no match
class HasParams subset set
instance HasParams (Proxy '[]) (Fixture bs)
instance (HasParam (Proxy a) (Fixture bs), HasParams (Proxy as) (Fixture bs)) => HasParam (Proxy (a ': as)) (Fixture bs)



{-
class SetParam (Proxy param) (Fixture fixtureParams) where
  setParam :: Fixture fixtureParams
-}



setParams :: (HasParams (Proxy params) (Fixture fixtureParams)) => Proxy params -> Fixture fixtureParams -> Frame -> Frame
setParams params fixture frame
  = frame
-}
{-
data RGB = RGB
 { _red   :: Double
 , _green :: Double
 , _blue  :: Double
 }
 deriving (Eq, Ord, Read, Show)

data RGBAWUV = RGBAWUV
 { _red'   :: Double
 , _green' :: Double
 , _blue'  :: Double
 , _amber' :: Double
 , _white' :: Double
 , _uv'    :: Double
 }
 deriving (Eq, Ord, Read, Show)

data Fixture = Fixture
 { elements :: Element
 }
 deriving (Eq, Ord, Read, Show)

data Hex12p = Hex_7Channel
 { hex12p :: RGBAWUV
 }
-}
