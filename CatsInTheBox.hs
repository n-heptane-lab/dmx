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

import CatSequences (cylon, cylon2, cylon3, masters, strobeWhite, hex12p_1, hex12p_2, hslRed, hslRedOrange, hslBlue, hslCyan, hslPurple, hslPink, hslYellow, hslGreen, hslBlack, randomUltra, randomUltra2, ultraAlternate, ultrabar_1, ultrabar_2, universe, ultra1234, ultra1234_accum, ultra12345678, pong2, redBlueU, pulsar)
import Color (HSL(..), RGB(..), hsl2rgb, rgb2hsl, rgb_d2w)
import Core
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
import FRP.Netwire.Analyze (lAvg)
import FRP.Netwire.Move (derivative, integral)
import FRP.Netwire.Noise (noiseR)
import GHC.TypeLits -- (TypeError, ErrorMessage(..), Nat, Symbol, KnownNat(..), natVal, natVal')
import GHC.Exts
import Hex12p
import Prelude hiding ((.), id, until, mapM)
import System.Environment (getArgs)
import System.Hardware.Serialport as Serialport (CommSpeed(CS9600, CS115200), SerialPort, SerialPortSettings(commSpeed), closeSerial, defaultSerialSettings, openSerial, recv, send, flush)
import System.MIDI as MIDI
import System.MIDI.Utility as MIDI
import System.Random (Random, StdGen, mkStdGen, randomR)


{-
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
  , (11, fireworks1)
  , (12, fireworks2)
  , (13, ultraFlicker 0 0)
  , (14, tanPars)
  , (15, cylon sixteenth (rgbWhite 255))
  , (16, ultraFlicker' quarter 41 1) -- ultraFlicker orange
  , (17, flickerHSL whole 34 1) -- flicker orange
  , (18, strobeWhite sixteenth 3)
  , (19, flickerHSL half 0 0)
  , (20, unisonWhitePulse quarter)
  , (21, blueDerby 20) -- slowBlueDerby
  , (22, ultraFlicker' half 70 0.75) -- ultraFlicker yellow
  , (23, blueGreenBackground)
  , (24, blueGreenBackgroundPulse whole)
  , (25, ultraFlicker' half 0 0) -- ultraFlicker slow white
  , (26, hexUV 255)
  , (27, slowBlueGreenDerby 40)
  , (28, ultrabarSolid (HSL 240 1 0.5)) -- ultrabar blue
  , (29, ultraFlicker' whole 240 1) -- ultrabar blue flicker
  , (30, greenDerby 50)
  , (31, static 1)
  , (32, static 0.25)
  , (33, purplePars)
  , (34, ultraStatic (0.8,1) thirtysecondth 0 0)
  , (35, blueDerby 200) -- fast BlueDerby
  , (36, ultraStatic (0.3,1) sixteenth 0 0)
  , (37, blueishGreenGigBar)
  , (38, gbStatic (0.1, 1) sixteenth 0 0)
  , (39, ultraStatic (0.5,1) thirtysecondth 0 0)
  , (40, blueYellowPars)
  , (41, rgbDerby 70)
  , (42, hexStatic (50,255) eighth red)
  , (43, slimParStatic (50, 255) sixteenth blue)
  , (44, unisonHSLPulse 170 1 quarter)
  , (45, ultraFlicker' eighth 298 1 ) -- ultraFlicker' magenta
  , (46, purpleTealPars)
  , (47, blueGreenBackgroundPulse2 whole)
  , (48, ultraFlicker' quarter 160 1) -- ultraFlicker teal
  , (49, slimHSLPulse 260 1 whole) -- slim purple pulse
  , (50, hexStatic (0,200) 2 amber)
  , (51, ultraStatic (0,1) 1 0 0) -- ultraStatic fast
  , (52, gbStatic (0,0.3) 1 0 0) -- gbStatic fast
  , (53, greenishGigBar)
  , (54, ultraFlicker' whole 298 1 ) -- ultraFlicker' magenta
  , (55, leaves)
  , (56, hexStatic (0, 200) 2 white) -- hexStatic white
  , (57, gbStatic (0,0.3) 3 120 1) -- gbStatic green fast
  , (58, gbLasers1)
  ]
-}

------------------------------------------------------------------------
-- Modes
------------------------------------------------------------------------

catInTheBoxModes :: Map Int MidiLights
catInTheBoxModes = Map.fromList
  [ (0, strobeWhite quarter 3)
  , (1, (cylon3 (eighth) hslCyan (select ultrabar_1 universe) >>> delay' eighth 0.25 ))
  , (2, (cylon3 (eighth) hslPink (Mirrored $ select ultrabar_2 universe) >>> delay' eighth 0.25))
  , (3, (cylon3 (eighth) (hsl $ HSL 240 1 0.5) (Mirrored $ select ultrabar_1 universe) >>> delay' eighth 0.25))
  , (4, (randomUltra [hslBlack, hslRed, hslBlue, hslCyan, hslPink] quarter universe))
  , (5, (randomUltra2 [hslBlack, hslRed, hslBlue, hslCyan, hslPink] eighth eighth universe))
  , (6, (randomUltra [hslRed, hslBlue, hslCyan, hslPink, hslBlack] eighth universe))
  , (7, (ultraAlternate hslRedOrange hslBlue eighth universe))
  ]

------------------------------------------------------------------------
-- Main
------------------------------------------------------------------------

main :: IO ()
main =
  do queue <- atomically newTQueue
     bracket (createDestination "DMX" (Just $ callback queue)) (\c -> putStrLn "disposing of connection." >> disposeConnection c) $ \midiIn ->
       bracket_ (start midiIn) (putStrLn "closing midi" >> MIDI.close midiIn) $
        withSerial dmxPort dmxBaud $ \s ->
         do putStrLn "running..."
--            runShow queue (serialOutput s) beatSession (midiModes catInTheBoxModes)
            runShow queue (serialOutput s) beatSession $
             proc e ->
               do -- c1 <- (cylon3 (eighth) hslCyan (select ultrabar_1 universe) >>> delay' eighth 0.25)  -< e
--                  c2 <- (cylon3 (eighth) hslPink (Mirrored $ select ultrabar_2 universe) >>> delay' eighth 0.25) -< e
--                  c2 <- (cylon3 (eighth) hslPink (Mirrored $ select ultrabar_2 universe)) -< e
--                  c1 <- (ultra1234_accum (quarter) (hslPink, hslPink, hslCyan, hslCyan, hslYellow, hslYellow)  (select ultrabar_1 universe)) -< e
--                  c2 <- (ultra1234 (quarter) (hslPink, hslPink, hslCyan, hslCyan, hslYellow, hslYellow)  (select ultrabar_2 universe)) -< e
--                  c2 <- (ultra12345678 (eighth) (hslGreen, hslGreen, hslGreen, hslGreen, hslGreen, hslGreen)  (Mirrored $ (select ultrabar_2 universe))  >>> delay' eighth 0.25) -< e
--                  c2' <- (ultra12345678 (eighth) (hslPurple, hslPurple, hslPurple, hslPurple, hslPurple, hslPurple)  ((select ultrabar_2 universe)) ) -< e
                  u1 <- (pong2 (sixteenth) (hslRed, hslRed)  (select ultrabar_2 universe)) -< e
                  u2 <- (pong2 (sixteenth) (hslRed, hslRed)  (select ultrabar_1 universe)) -< e
--                  c3 <- strobeWhite quarter 10 -< e
                  h1 <- redBlueU (select hex12p_1 universe) -< e
--                  h2 <- redBlueU (select hex12p_2 universe) >>> delay'' quarter 1 -< e
--                  h2 <- redBlueU (select hex12p_2 universe) -< e
                  m <- masters -< e
                  h <- pulsar (select hex12p_1 universe  :+: select hex12p_2 universe ) -< e
                  returnA -< mergeParamsL [m, h, u1, u2]


