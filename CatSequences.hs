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
import Data.Word (Word8)
import Data.Vector.Mutable (IOVector, write)
import qualified Data.Vector.Mutable as MVector
import Hex12p
import Prelude hiding ((.), id, until, mapM)
import Ultrabar
import System.Random (Random, StdGen, mkStdGen, randomR)

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
-- Colors
------------------------------------------------------------------------

hslRed       = hsl $ HSL   0 1 0.5
hslRedOrange = hsl $ HSL  15 1 0.5
hslYellow    = hsl $ HSL  60 1 0.5
hslGreen     = hsl $ HSL 120 1 0.3
hslCyan      = hsl $ HSL 190 1 0.5
hslBlue      = hsl $ HSL 240 1 0.5
hslPurple    = hsl $ HSL 280 1 0.5
hslPink      = hsl $ HSL 320 1 0.5
hslBlack     = hsl $ HSL   0 1 0

------------------------------------------------------------------------
-- Sequences
------------------------------------------------------------------------
{-
masters :: (SetParam 'Master (SubUniverse Hex12p_1 u), Select Hex12p_1 u, SetParam 'Master (SubUniverse Hex12p_2 u), Select Hex12p_2 u) =>
           u -> MidiLights
masters u =
  let h1 = select hex12p_1 u
      h2 = select hex12p_2 u
  in proc e ->
       do m <- pure $ setParam (master 255) (h1 :+: h2) -< e
          returnA -< m
-}
masters :: MidiLights
masters =
  proc e ->
      do m1  <- pure $ setParam (master 255) (select hex12p_1 universe :+: select hex12p_2 universe) -< e
         returnA -< mergeParamsL [m1]

strobeWhite :: Int -> Int -> MidiLights
strobeWhite dur p =
  proc e ->
    do -- m <- masters su -< e
       m <- masters -< e
       p <- lfo (PWM p) dur 0 -< e
       returnA -< concat [ m
                         , setParam (white (round (p * 255))) (select hex12p_1 universe)
                         , setParam (white (round (p * 255))) (select hex12p_2 universe)
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

ultra1234_accum dur' ps@(p0, p1, p2, p3, p4, p5) ultra =
  let dur  = dur' + 1
  in (for dur . pure []) -->
     (for dur . pure (mergeParamsL [ setParams p0 (select at0 ultra)
                                   , setParams p1 (select at1 ultra)
                                   ]))  -->
     (for dur . pure (mergeParamsL [ setParams p0 (select at0 ultra)
                                   , setParams p1 (select at1 ultra)
                                   , setParams p2 (select at2 ultra)
                                   , setParams p3 (select at3 ultra)
                                   ]))  -->
     (for dur . pure (mergeParamsL [ setParams p0 (select at0 ultra)
                                   , setParams p1 (select at1 ultra)
                                   , setParams p2 (select at2 ultra)
                                   , setParams p3 (select at3 ultra)
                                   , setParams p4 (select at4 ultra)
                                   , setParams p5 (select at5 ultra)
                                   ]))  -->
     ultra1234_accum dur' ps ultra

ultra1234 dur' p ultra =
  let dur  = dur' + 1
  in (for dur . pure (setParams p (select at0 ultra))) -->
     (for dur . pure (setParams p (select at1 ultra))) -->
     (for dur . pure (setParams p (select at2 ultra))) -->
     (for dur . pure (setParams p (select at3 ultra))) -->
     ultra1234 dur' p ultra

ultra12345678 dur' ps@(p0, p1, p2, p3, p4, p5)  ultra =
  let dur  = dur' + 1
  in (for dur . pure (setParams p0 (select at0 ultra))) -->
     (for dur . pure (setParams p1 (select at1 ultra))) -->
     (for dur . pure (setParams p2 (select at2 ultra))) -->
     (for dur . pure (setParams p3 (select at3 ultra))) -->
     (for dur . pure (setParams p4 (select at4 ultra))) -->
     (for dur . pure (setParams p5 (select at5 ultra))) -->
     (for (dur * 2) . pure []) -->
     ultra12345678 dur' ps ultra


pong2 dur' ps@(p0, p1) ultra =
  let dur  = dur' + 1
  in (for dur . pure (mergeParamsL [ setParams p0 (select at0 ultra)
                                   , setParams p1 (select at1 ultra)
                                   ]))  -->
     (for dur . pure (mergeParamsL [ setParams p0 (select at1 ultra)
                                   , setParams p1 (select at2 ultra)
                                   ]))  -->
     (for dur . pure (mergeParamsL [ setParams p0 (select at2 ultra)
                                   , setParams p1 (select at3 ultra)
                                   ]))  -->
     (for dur . pure (mergeParamsL [ setParams p0 (select at3 ultra)
                                   , setParams p1 (select at4 ultra)
                                   ]))  -->
     (for dur . pure (mergeParamsL [ setParams p0 (select at4 ultra)
                                   , setParams p1 (select at5 ultra)
                                   ]))  -->
     (for dur . pure (mergeParamsL [ setParams p0 (select at3 ultra)
                                   , setParams p1 (select at4 ultra)
                                   ]))  -->
     (for dur . pure (mergeParamsL [ setParams p0 (select at2 ultra)
                                   , setParams p1 (select at3 ultra)
                                   ]))  -->
     (for dur . pure (mergeParamsL [ setParams p0 (select at1 ultra)
                                   , setParams p1 (select at2 ultra)
                                   ]))  -->
     pong2 dur' ps ultra


{-
cylon3 dur' p ultra =
  let dur = dur' + 1
  in (for dur . pure (setParams p (select at5 ultra))) -->
     cylon3 dur' p ultra
-}


randomUltra :: [Param 'Red :+: Param 'Green :+: Param 'Blue] -> Int -> Universe -> MidiLights
randomUltra options holdTime universe =
  let range = (0, (length options) - 1)
      ultra1 = select ultrabar_1 universe
      ultra2 = select ultrabar_2 universe
  in
  proc e ->
       do r0  <- randomD (mkStdGen 602) (range) >>> periodic holdTime >>> hold -< ()
          r1  <- randomD (mkStdGen 123) (range) >>> periodic holdTime >>> hold -< ()
          r2  <- randomD (mkStdGen 125) (range) >>> periodic holdTime >>> hold -< ()
          r3  <- randomD (mkStdGen 121) (range) >>> periodic holdTime >>> hold -< ()
          r4  <- randomD (mkStdGen 120) (range) >>> periodic holdTime >>> hold -< ()
          r5  <- randomD (mkStdGen 224) (range) >>> periodic holdTime >>> hold -< ()
          r6  <- randomD (mkStdGen 502) (range) >>> periodic holdTime >>> hold -< ()
          r7  <- randomD (mkStdGen 223) (range) >>> periodic holdTime >>> hold -< ()
          r8  <- randomD (mkStdGen 225) (range) >>> periodic holdTime >>> hold -< ()
          r9  <- randomD (mkStdGen 221) (range) >>> periodic holdTime >>> hold -< ()
          r10 <- randomD (mkStdGen 220) (range) >>> periodic holdTime >>> hold -< ()
          r11 <- randomD (mkStdGen 324) (range) >>> periodic holdTime >>> hold -< ()

          returnA -< concat [ setParams (options!!r0)  (select at0 ultra1)
                            , setParams (options!!r1)  (select at1 ultra1)
                            , setParams (options!!r2)  (select at2 ultra1)
                            , setParams (options!!r3)  (select at3 ultra1)
                            , setParams (options!!r4)  (select at4 ultra1)
                            , setParams (options!!r5)  (select at5 ultra1)
                            , setParams (options!!r6)  (select at0 ultra2)
                            , setParams (options!!r7)  (select at1 ultra2)
                            , setParams (options!!r8)  (select at2 ultra2)
                            , setParams (options!!r9)  (select at3 ultra2)
                            , setParams (options!!r10) (select at4 ultra2)
                            , setParams (options!!r11) (select at5 ultra2)
                            ]

randomUltra2 :: [Param 'Red :+: Param 'Green :+: Param 'Blue] -> Int -> Int -> Universe -> MidiLights
randomUltra2 options holdTime offTime universe =
  let range = (0, (length options) - 1)
      ultra1 = select ultrabar_1 universe
      ultra2 = select ultrabar_2 universe
      blink v = v >>> blink'
      blink' = for (holdTime + 1) . id --> for (offTime + 1) . pure 0 --> blink'
  in
  proc e ->
       do r0  <- blink (randomD (mkStdGen 602) (range) >>> periodic holdTime >>> hold) -< ()
          r1  <- blink (randomD (mkStdGen 123) (range) >>> periodic holdTime >>> hold) -< ()
          r2  <- blink (randomD (mkStdGen 125) (range) >>> periodic holdTime >>> hold) -< ()
          r3  <- blink (randomD (mkStdGen 121) (range) >>> periodic holdTime >>> hold) -< ()
          r4  <- blink (randomD (mkStdGen 120) (range) >>> periodic holdTime >>> hold) -< ()
          r5  <- blink (randomD (mkStdGen 224) (range) >>> periodic holdTime >>> hold) -< ()
          r6  <- blink (randomD (mkStdGen 502) (range) >>> periodic holdTime >>> hold) -< ()
          r7  <- blink (randomD (mkStdGen 223) (range) >>> periodic holdTime >>> hold) -< ()
          r8  <- blink (randomD (mkStdGen 225) (range) >>> periodic holdTime >>> hold) -< ()
          r9  <- blink (randomD (mkStdGen 221) (range) >>> periodic holdTime >>> hold) -< ()
          r10 <- blink (randomD (mkStdGen 220) (range) >>> periodic holdTime >>> hold) -< ()
          r11 <- blink (randomD (mkStdGen 324) (range) >>> periodic holdTime >>> hold) -< ()

          returnA -< concat [ setParams (options!!r0)  (select at0 ultra1)
                            , setParams (options!!r1)  (select at1 ultra1)
                            , setParams (options!!r2)  (select at2 ultra1)
                            , setParams (options!!r3)  (select at3 ultra1)
                            , setParams (options!!r4)  (select at4 ultra1)
                            , setParams (options!!r5)  (select at5 ultra1)
                            , setParams (options!!r6)  (select at0 ultra2)
                            , setParams (options!!r7)  (select at1 ultra2)
                            , setParams (options!!r8)  (select at2 ultra2)
                            , setParams (options!!r9)  (select at3 ultra2)
                            , setParams (options!!r10) (select at4 ultra2)
                            , setParams (options!!r11) (select at5 ultra2)
                            ]

ultraAlternate :: (Param 'Red :+: Param 'Green :+: Param 'Blue) -> (Param 'Red :+: Param 'Green :+: Param 'Blue) -> Int -> Universe -> MidiLights
ultraAlternate left right duration' universe =
  let duration = duration' + 1
  in
    for duration . pure (setParams left (select ultrabar_1 universe)) -->
    for duration . pure (setParams right (select ultrabar_2 universe)) -->
    ultraAlternate left right duration' universe


-- * Hex
{-
redBlue :: universe MidiLights
redBlue =
  let dur = quarter + 1
      u = (select hex12p1 universe)
  in
  for dur . pure (concat [setParam (red 255) u, setParam (master 255) u]) -->
  for dur . pure (concat [setParam (blue 255) u, setParam (master 255) u]) -->
  redBlue
-} 
-- redBlueU :: (SetParam 'Red universe, SetParam 'Blue universe) => universe -> MidiLights
redBlueU u =
  let dur = quarter + 1 in
  for dur . pure (setParams hslRed u) -->
  for dur . pure (setParams hslBlue u) -->
  redBlueU u


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

