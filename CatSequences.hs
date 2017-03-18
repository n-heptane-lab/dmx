{-# language Arrows #-}
{-# language DataKinds #-}
{-# language FlexibleContexts #-}
{-# language TypeOperators #-}
{-# language UndecidableInstances #-}
module CatSequences where

import Control.Arrow
import Control.Wire hiding ((<>))
import Control.Wire.Core
import Core
import Color (HSL(..), RGB(..), hsl2rgb, rgb2hsl, rgb_d2w)
import Data.Proxy (Proxy(..))
import Data.Sequence (Seq, (<|), ViewR(..), viewr)
import qualified Data.Sequence as Seq
import Data.Word (Word8)
import Data.Vector.Mutable (IOVector, write)
import qualified Data.Vector.Mutable as MVector
import Hex12p
import Prelude hiding ((.), id, until, mapM)
import Ultrabar

------------------------------------------------------------------------
-- Hex 12P
------------------------------------------------------------------------

type Def_Hex12p_1 = Labeled "hex12p_1" (Fixture Hex12p_7channel)

def_hex12p_1 :: Def_Hex12p_1
def_hex12p_1 = Labeled $ hex12p 1

type Hex12p_1 = Label "hex12p_1"

hex12p_1 :: Proxy Hex12p_1
hex12p_1 = Proxy

type Def_Hex12p_2 = Labeled "hex12p_2" (Fixture Hex12p_7channel)

def_hex12p_2 :: Def_Hex12p_2
def_hex12p_2 = Labeled $ hex12p 8

type Hex12p_2 = Label "hex12p_2"

hex12p_2 :: Proxy Hex12p_2
hex12p_2 = Proxy

------------------------------------------------------------------------
-- ultrabar
------------------------------------------------------------------------

type Def_Ultrabar_1 = Labeled "ultrabar_1" Ultrabar_18

def_ultrabar_1 :: Def_Ultrabar_1
def_ultrabar_1 = Labeled $ ultrabar 15

type Ultrabar_1 = Label "ultrabar_1"

ultrabar_1 :: Proxy Ultrabar_1
ultrabar_1 = Proxy

type Def_Ultrabar_2 = Labeled "ultrabar_2" Ultrabar_18

def_ultrabar_2 :: Def_Ultrabar_2
def_ultrabar_2 = Labeled $ ultrabar 33

type Ultrabar_2 = Label "ultrabar_2"

ultrabar_2 :: Proxy Ultrabar_2
ultrabar_2 = Proxy

------------------------------------------------------------------------
-- Universe
------------------------------------------------------------------------

type Universe = Def_Hex12p_1 :+: Def_Hex12p_2 :+: Def_Ultrabar_1 :+: Def_Ultrabar_2

universe :: Universe
universe = def_hex12p_1 :+: def_hex12p_2 :+: def_ultrabar_1 :+: def_ultrabar_2

------------------------------------------------------------------------
-- Sequences
------------------------------------------------------------------------

masters :: (SetParam 'Master (SubUniverse Hex12p_1 u), Select Hex12p_1 u, SetParam 'Master (SubUniverse Hex12p_2 u), Select Hex12p_2 u) =>
           u -> MidiLights
masters u =
  let h1 = select hex12p_1 u
      h2 = select hex12p_2 u
  in proc e ->
       do m <- pure $ setParam (master 255) (h1 :+: h2) -< e
          returnA -< m
strobeWhite
  :: (SetParam 'Master universe,
      SetParam 'White universe) =>
     Int -> Int -> universe -> MidiLights
strobeWhite dur p su =
  proc e ->
    do -- m <- masters su -< e
       p <- lfo (PWM p) dur 0 -< e
       returnA -< mergeParamsL [ setParam (white (round (p * 255))) su
                               , setParam (master 255) su
                               ]

cylon dur' p ultra =
  let dur = dur' + 1
  in (for ((dur' * 2)+1) . pure (setParams p (select at0 ultra))) -->
     (for dur . pure (setParams p (select at1 ultra))) -->
     (for dur . pure (setParams p (select at2 ultra))) -->
     (for dur . pure (setParams p (select at3 ultra))) -->
     (for dur . pure (setParams p (select at4 ultra))) -->
     (for ((dur' * 4)+1) . pure (setParams p (select at5 ultra))) -->
     (for dur . pure (setParams p (select at4 ultra))) -->
     (for dur . pure (setParams p (select at3 ultra))) -->
     (for dur . pure (setParams p (select at2 ultra))) -->
     (for dur . pure (setParams p (select at1 ultra))) -->
     (for ((dur' * 2)+1) . pure (setParams p (select at0 ultra))) -->
     cylon dur' p ultra


cylon2 dur' p ultra =
  let dur = dur' + 1
  in (for dur . pure (setParams p (select at0 ultra))) -->
     (for dur . pure (setParams p (select at1 ultra))) -->
     (for dur . pure (setParams p (select at2 ultra))) -->
     (for dur . pure (setParams p (select at3 ultra))) -->
     (for dur . pure (setParams p (select at4 ultra))) -->
     (for dur . pure (setParams p (select at5 ultra))) -->
     (for dur . pure (setParams p (select at5 ultra))) -->
     (for dur . pure (setParams p (select at4 ultra))) -->
     (for dur . pure (setParams p (select at3 ultra))) -->
     (for dur . pure (setParams p (select at2 ultra))) -->
     (for dur . pure (setParams p (select at1 ultra))) -->
     (for dur . pure (setParams p (select at0 ultra))) -->
     cylon2 dur' p ultra


cylon3 dur' p ultra =
  let dur = dur' + 1
  in (for dur . pure (setParams p (select at0 ultra))) -->
     (for dur . pure (setParams p (select at1 ultra))) -->
     (for dur . pure (setParams p (select at2 ultra))) -->
     (for dur . pure (setParams p (select at3 ultra))) -->
     (for dur . pure (setParams p (select at4 ultra))) -->
     (for dur . pure (setParams p (select at5 ultra))) -->
     (for dur . pure []) -->
     (for dur . pure []) -->
     (for dur . pure (setParams p (select at5 ultra))) -->
     (for dur . pure (setParams p (select at4 ultra))) -->
     (for dur . pure (setParams p (select at3 ultra))) -->
     (for dur . pure (setParams p (select at2 ultra))) -->
     (for dur . pure (setParams p (select at1 ultra))) -->
     (for dur . pure (setParams p (select at0 ultra))) -->
     (for dur . pure []) -->
     (for dur . pure []) -->

     cylon3 dur' p ultra

{-
cylon3 dur' p ultra =
  let dur = dur' + 1
  in (for dur . pure (setParams p (select at5 ultra))) -->
     cylon3 dur' p ultra
-}
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
