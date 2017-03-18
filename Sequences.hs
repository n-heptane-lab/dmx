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

masters :: MidiLights
masters =
  let par1 = (select gb_par_1 (select gb_1 universe))
      par2 = (select gb_par_2 (select gb_1 universe))
--      laser = select gb_laser (select gb_1 universe)
  in proc e ->
      do m1  <- pure $ setParam (master 255) (select slimPar64_1 universe :+: select hex12p1 universe) -< e
         m2  <- pure $ setParam (parCon $ ParConRGB 127) (par1 :+: par2) -< e
--         l  <- laserOff laser -< e
         returnA -< mergeParamsL [m1, m2]

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

fireworks1 =
  proc e ->
   do m <- masters -< e
      l1 <- lfo Ramp eighth 0 -< e
      l2 <- (for (thirtysecondth) . pure 0) --> lfo Ramp eighth 0 -< e
      l3 <- (for (2*thirtysecondth) . pure 0) --> lfo Ramp eighth 0 -< e
      l4 <- (for (2*thirtysecondth) . pure 0) --> lfo Ramp eighth 0 -< e
      returnA -< concat [ m
                        , setParams (hsl $ HSL 39 1 (0.61 * l1)) (select slimPar64_1 universe)
                        , setParams (hsl $ HSL 39 1 (0.61 * l2)) (select gb_par_1 (select gb_1 universe) :+: select gb_par_2 (select gb_1 universe))
                        , setParams (hsl $ HSL 39 1 (0.61 * l3)) (select ultrabar_1 universe)
                        , setParam (amber (round (l4 * l4 * 255))) (select hex12p1 universe)
                        ]

ultraFlicker h s =
  let dur = quarter
      ultra = select ultrabar_1 universe
  in proc e ->
       do p1 <- lfo Sine dur 0 -< e
          p2 <- (for sixteenth . pure 0) --> lfo Sine dur 0 -< e
          p3 <- (for (sixteenth * 2) . pure 0) --> lfo Sine dur 0 -< e
          p4 <- (for (sixteenth * 3) . pure 0) --> lfo Sine dur 0 -< e
          p5 <- (for (sixteenth * 4) . pure 0) --> lfo Sine dur 0 -< e
          p6 <- (for (sixteenth * 5) . pure 0) --> lfo Sine dur 0 -< e
          returnA -< concat [ setParams (hsl $ HSL h s ((p1 + 1) / 2)) (select at0 ultra)
                            , setParams (hsl $ HSL h s ((p2 + 1) / 2)) (select at1 ultra)
                            , setParams (hsl $ HSL h s ((p3 + 1) / 2)) (select at2 ultra)
                            , setParams (hsl $ HSL h s ((p4 + 1) / 2)) (select at3 ultra)
                            , setParams (hsl $ HSL h s ((p5 + 1) / 2)) (select at4 ultra)
                            , setParams (hsl $ HSL h s ((p6 + 1) / 2)) (select at5 ultra)
                            ]

ultraFlicker' dur h s =
  let ultra = select ultrabar_1 universe
  in proc e ->
       do p1 <- lfo Sine dur 0 -< e
          p2 <- (for sixteenth . pure 0) --> lfo Sine dur 0 -< e
          p3 <- (for (sixteenth * 2) . pure 0) --> lfo Sine dur 0 -< e
          p4 <- (for (sixteenth * 3) . pure 0) --> lfo Sine dur 0 -< e
          p5 <- (for (sixteenth * 4) . pure 0) --> lfo Sine dur 0 -< e
          p6 <- (for (sixteenth * 5) . pure 0) --> lfo Sine dur 0 -< e
          returnA -< concat [ setParams (hsl $ HSL h s (((p1 + 1) / 4))) (select at0 ultra)
                            , setParams (hsl $ HSL h s (((p2 + 1) / 4))) (select at1 ultra)
                            , setParams (hsl $ HSL h s (((p3 + 1) / 4))) (select at2 ultra)
                            , setParams (hsl $ HSL h s (((p4 + 1) / 4))) (select at3 ultra)
                            , setParams (hsl $ HSL h s (((p5 + 1) / 4))) (select at4 ultra)
                            , setParams (hsl $ HSL h s (((p6 + 1) / 4))) (select at5 ultra)
                            ]

tanPars :: MidiLights
tanPars =
  proc e ->
    do m <- masters -< e
       t <- pure $ setParams (hsl (HSL 41 0.78  0.73)) (select gb_par_1 (select gb_1 universe) :+: select gb_par_2 (select gb_1 universe)) -< e
       returnA -< mergeParamsL [ m, t ]

cylon dur' p =
  let dur = dur' + 1
      ultra = select ultrabar_1 universe
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
     cylon dur' p

fireworks2 =
  proc e ->
   do m <- masters -< e
      l1 <- lfo Ramp half 0 -< e
      l2 <- (for (eighth) . pure 0) --> lfo Ramp half 0 -< e
      l3 <- (for (2*eighth) . pure 0) --> lfo Ramp half 0 -< e
      l4 <- (for (2*eighth) . pure 0) --> lfo Ramp half 0 -< e
      returnA -< concat [ m
                        , setParams (hsl $ HSL 323 1 (0.61 * l1)) (select slimPar64_1 universe)
                        , setParams (hsl $ HSL 323 1 (0.61 * l2)) (select gb_par_1 (select gb_1 universe) :+: select gb_par_2 (select gb_1 universe))
                        , setParams (hsl $ HSL 323 1 (0.61 * l3)) (select ultrabar_1 universe)
                        , setParam (green (round (l4 * l4 * 255))) (select hex12p1 universe)
                        ]

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

flickerHSL dur h s =
  do proc e ->
       do m <- masters -< e
          p1 <- lfo Sine dur 0 -< e
          p2 <- lfo Sine dur (3*pi/2) -< e
          p3 <- lfo Sine dur pi -< e
          p4 <- lfo Sine dur (pi/2) -< e
          returnA -< concat [ m
                            , setParams (hsl $ HSL h s (0.3+(p1*0.2))) (select hex12p1 universe)
                            , setParams (hsl $ HSL h s (0.3+(p3*0.2))) (select slimPar64_1 universe)
                            , setParams (hsl $ HSL h s (0.3+(p2*0.2))) (select gb_par_1 (select gb_1 universe))
                            , setParams (hsl $ HSL h s (0.3+(p4*0.2))) (select gb_par_2 (select gb_1 universe))
                            ]

unisonWhitePulse dur =
  do proc e ->
       do m <- masters -< e
          p1 <- lfo Sine dur 0 -< e
          v <- arr (\p -> hsl (HSL 0 0 (0.3+(p * 0.2)))) -< p1
          returnA -< concat [ m
                            , setParams v  (select hex12p1 universe)
                            , setParams v (select slimPar64_1 universe)
                            , setParams v (select gb_par_1 (select gb_1 universe))
                            , setParams v (select gb_par_2 (select gb_1 universe))
                            ]

strobeWhite dur p =
  proc e ->
    do m <- masters -< e
       p <- lfo (PWM p) dur 0 -< e
       returnA -< concat [ m
                         , setParam (white (128 + round (p * 127))) (select hex12p1 universe)
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


-- * My Love, My Friend

blueDerby :: Word8 -> MidiLights
blueDerby speed =
  proc e ->
    do d1  <- pure (setParams ((derbyCon DerbyConBlue) :+: (derbyRotation (DerbyClockwise speed))) (select gb_derby_1 (select gb_1 universe))) -< e
       d2  <- pure (setParams ((derbyCon DerbyConBlue) :+: (derbyRotation (DerbyCounterClockwise speed))) (select gb_derby_2 (select gb_1 universe))) -< e
       returnA -< mergeParamsL [d1, d2]

slowBlueGreenDerby :: Word8 -> MidiLights
slowBlueGreenDerby v =
  proc e ->
    do d1  <- pure (setParams ((derbyCon DerbyConGreenBlue) :+: (derbyRotation (DerbyClockwise v))) (select gb_derby_1 (select gb_1 universe))) -< e
       d2  <- pure (setParams ((derbyCon DerbyConGreenBlue) :+: (derbyRotation (DerbyCounterClockwise v))) (select gb_derby_2 (select gb_1 universe))) -< e
       returnA -< mergeParamsL [d1, d2]

greenDerby :: Word8 -> MidiLights
greenDerby v =
  proc e ->
    do d1  <- pure (setParams ((derbyCon DerbyConGreen) :+: (derbyRotation (DerbyClockwise v))) (select gb_derby_1 (select gb_1 universe))) -< e
       d2  <- pure (setParams ((derbyCon DerbyConGreen) :+: (derbyRotation (DerbyCounterClockwise v))) (select gb_derby_2 (select gb_1 universe))) -< e
       returnA -< mergeParamsL [d1, d2]

blueGreenBackground :: MidiLights
blueGreenBackground =
  proc e ->
    do m <- masters -< e
       pars <- pure $ setParams (hsl $ HSL 142 1 0.5)  (select gb_par_1 (select gb_1 universe) :+: select gb_par_2 (select gb_1 universe)) -< e
--        pars <- pure $ setParams (green 255 :+: blue 30)  (select gb_par_1 (select gb_1 universe) :+: select gb_par_2 (select gb_1 universe)) -< e
       h <- pure $ setParam (uv 255) (select hex12p1 universe) -< e
       slim <- pure $ setParams (hsl $ HSL 222 1 0.3) (select slimPar64_1 universe) -< e
       returnA -< mergeParamsL  [m, h, pars, slim]


blueGreenBackgroundPulse :: Int -> MidiLights
blueGreenBackgroundPulse dur =
  proc e ->
    do m <- masters -< e
--       h <- pure $ setParam (uv 255) (select hex12p1 universe) -< e
       pars <- (lfo Sine dur 0) >>> (arr $ \p -> (setParams (hsl $ HSL 142 1 (0.3 + (p * 0.2)))  (select gb_par_1 (select gb_1 universe) :+: select gb_par_2 (select gb_1 universe)))) -< e
       slim <- (lfo Sine dur pi) >>> (arr $ \p -> (setParams (hsl $ HSL 132 1 (0.3 + (p * 0.2))) (select slimPar64_1 universe))) -< e
--       slim <- pure $ setParams (hsl $ HSL 222 1 0.3) (select slimPar64_1 universe) -< e
       returnA -< mergeParamsL  [m, pars, slim]

blueGreenBackgroundPulse2 :: Int -> MidiLights
blueGreenBackgroundPulse2 dur =
  proc e ->
    do m <- masters -< e
--       h <- pure $ setParam (uv 255) (select hex12p1 universe) -< e
       pars <- (lfo Sine dur 0) >>> (arr $ \p -> (setParams (hsl $ HSL  120 1 (0.3 + (p * 0.2)))  (select gb_par_1 (select gb_1 universe) :+: select gb_par_2 (select gb_1 universe)))) -< e
       slim <- (lfo Sine dur pi) >>> (arr $ \p -> (setParams (hsl $ HSL 180 1 (0.3 + (p * 0.2))) (select slimPar64_1 universe))) -< e
--       slim <- pure $ setParams (hsl $ HSL 222 1 0.3) (select slimPar64_1 universe) -< e
       returnA -< mergeParamsL  [m, pars, slim]

hexUV :: Int -> MidiLights
hexUV v =
  proc e ->
    do m <- masters -< e
       h <- pure $ setParam (uv 255) (select hex12p1 universe) -< e
       returnA -< mergeParamsL [m, h]

ultrabarSolid :: HSL Double -> MidiLights
ultrabarSolid v =
  proc e ->
    do m <- masters -< e
       h <- pure $ setParams (hsl v) (select ultrabar_1 universe) -< e
       returnA -< mergeParamsL [m, h]

static :: Double -> MidiLights
static scale =
  proc e ->
    do r1 <- randomD (mkStdGen 123) (0,1::Double) -< ()
       r2 <- randomD (mkStdGen 125) (0,1::Double) -< ()
       r3 <- randomD (mkStdGen 121) (0,1::Double) -< ()
       r4 <- randomD (mkStdGen 12) (0,1::Double) -< ()
       m <- masters -< e
       h <- arr (\r -> setParam (white (round (255 * scale * (r*r)))) (select hex12p1 universe)) -< r1
       p1 <- arr (\r -> setParams (rgbWhite (round (255 * scale * (r*r)))) (select gb_par_1 (select gb_1 universe))) -< r2
       p2 <- arr (\r -> setParams (rgbWhite (round (255 * scale * (r*r)))) (select gb_par_2 (select gb_1 universe))) -< r3
       s <- arr (\r -> setParams (rgbWhite (round (255 * scale * (r*r)))) (select slimPar64_1 universe)) -< r4
       returnA -< mergeParamsL [m, h, p1, p2, s]

purplePars :: MidiLights
purplePars =
  let purple = hsl $ HSL 274 1 0.5
  in
    proc e ->
      do m  <- masters -< e
         p1 <- arr (\r -> setParams purple (select gb_par_1 (select gb_1 universe))) -< e
         p2 <- arr (\r -> setParams purple (select gb_par_2 (select gb_1 universe))) -< e
         s  <- arr (\r -> setParams purple (select slimPar64_1 universe))            -< e
         returnA -< mergeParamsL [m, p1, p2, s]


ultraStatic :: (Double, Double) -> Int -> Double -> Double -> MidiLights
ultraStatic range holdTime h s =
  proc e ->
       do let ultra = select ultrabar_1 universe
          t <- arr fromIntegral <<< time -< ()
          r1 <- randomD (mkStdGen 123) (range) >>> periodic holdTime >>> hold -< ()
          r2 <- randomD (mkStdGen 125) (range) >>> periodic holdTime >>> hold -< ()
          r3 <- randomD (mkStdGen 121) (range) >>> periodic holdTime >>> hold -< ()
          r4 <- randomD (mkStdGen 120) (range) >>> periodic holdTime >>> hold -< ()
          r5 <- randomD (mkStdGen 224) (range) >>> periodic holdTime >>> hold -< ()
          r6 <- randomD (mkStdGen 502) (range) >>> periodic holdTime >>> hold -< ()
          returnA -< concat [ setParams (hsl $ HSL h s r1) (select at0 ultra)
                            , setParams (hsl $ HSL h s r2) (select at1 ultra)
                            , setParams (hsl $ HSL h s r3) (select at2 ultra)
                            , setParams (hsl $ HSL h s r4) (select at3 ultra)
                            , setParams (hsl $ HSL h s r5) (select at4 ultra)
                            , setParams (hsl $ HSL h s r6) (select at5 ultra)
                            ]

gbStatic :: (Double, Double) -> Int -> Double -> Double -> MidiLights
gbStatic range holdTime h s =
  proc e ->
       do let par1 = select gb_par_1 (select gb_1 universe)
              par2 = select gb_par_2 (select gb_1 universe)
          r1 <- randomD (mkStdGen 123) (range) >>> periodic holdTime >>> hold -< ()
          r2 <- randomD (mkStdGen 125) (range) >>> periodic holdTime >>> hold -< ()
          m <- masters -< e
          returnA -< concat [ m
                            , setParams (hsl $ HSL h s r1) par1
                            , setParams (hsl $ HSL h s r2) par2
                            ]

-- hexStatic :: (Double, Double) -> Int -> Double -> Double -> MidiLights
{-
hexStatic :: (Random a, SetParam param (Fixture Hex12p_7channel)) =>
             (a, a)
          -> Int
          -> (a -> param)
          -> MidiLights
-}
hexStatic range holdTime f =
  proc e ->
       do v <- randomD (mkStdGen 123) (range) >>> periodic holdTime >>> hold -< ()
          m <- masters -< e
          returnA -< concat [ m
                            , setParam (f v) (select hex12p1 universe)
                            ]

slimParStatic range holdTime f =
  proc e ->
       do v <- randomD (mkStdGen 123) (range) >>> periodic holdTime >>> hold -< ()
          m <- masters -< e
          returnA -< concat [ m
                            , setParam (f v) (select slimPar64_1 universe)
                            ]

blueishGreenGigBar :: MidiLights
blueishGreenGigBar =
  proc e ->
    do m <- masters -< e
       pars <- pure $ setParams (hsl $ HSL 181 1 0.5)  (select gb_par_1 (select gb_1 universe) :+: select gb_par_2 (select gb_1 universe)) -< e
       returnA -< mergeParamsL  [m, pars]

greenishGigBar :: MidiLights
greenishGigBar =
  proc e ->
    do m <- masters -< e
       pars <- pure $ setParams (hsl $ HSL 140 0.6 0.6)  (select gb_par_1 (select gb_1 universe) :+: select gb_par_2 (select gb_1 universe)) -< e
       returnA -< mergeParamsL  [m, pars]


blueYellowPars :: MidiLights
blueYellowPars =
  proc e ->
    do m <- masters -< e
       p <- pure $ setParams (hsl (HSL 181 1  0.75)) (select gb_par_1 (select gb_1 universe) :+: select gb_par_2 (select gb_1 universe)) -< e
       s <- pure $ setParams (hsl (HSL 57 1 0.6)) (select slimPar64_1 universe) -< e
       returnA -< mergeParamsL [ m, p, s ]

rgbDerby :: Word8 -> MidiLights
rgbDerby v =
  proc e ->
    do d1  <- pure (setParams ((derbyCon DerbyConRedGreenBlue) :+: (derbyRotation (DerbyClockwise v))) (select gb_derby_1 (select gb_1 universe))) -< e
       d2  <- pure (setParams ((derbyCon DerbyConRedGreenBlue) :+: (derbyRotation (DerbyCounterClockwise v))) (select gb_derby_2 (select gb_1 universe))) -< e
       returnA -< mergeParamsL [d1, d2]

unisonHSLPulse h s dur =
  do proc e ->
       do m <- masters -< e
          p1 <- lfo Sine dur 0 -< e
          v <- arr (\p -> hsl (HSL h s (0.3+(p * 0.2)))) -< p1
          returnA -< concat [ m
                            , setParams v (select hex12p1 universe)
                            , setParams v (select slimPar64_1 universe)
                            , setParams v (select gb_par_1 (select gb_1 universe))
                            , setParams v (select gb_par_2 (select gb_1 universe))
                            ]

purpleTealPars :: MidiLights
purpleTealPars =
  proc e ->
    do m <- masters -< e
       p <- pure $ setParams (hsl (HSL 285 0.7  0.5)) (select gb_par_1 (select gb_1 universe) :+: select gb_par_2 (select gb_1 universe)) -< e
       s <- pure $ setParams (hsl (HSL 176 0.6 0.5)) (select slimPar64_1 universe) -< e
       returnA -< mergeParamsL [ m, p, s ]

slimHSLPulse h s dur =
  do proc e ->
       do m <- masters -< e
          p1 <- lfo Sine dur 0 -< e
          v <- arr (\p -> hsl (HSL h s (0.3+(p * 0.2)))) -< p1
          returnA -< concat [ m
                            , setParams v (select gb_par_1 (select gb_1 universe))
                            , setParams v (select gb_par_2 (select gb_1 universe))
                            ]

leave u p =
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
         returnA -< setParams (hsl $ HSL h 0.7 0.5) u

leaves =
  proc e ->
    do -- f <- flame (select slimPar64_1 universe :+: select ultrabar_1 universe :+: select hex12p1 universe :+: select gb_par_1 (select gb_1 universe) :+: select gb_par_2 (select gb_1 universe)) -< e
       f1 <- flame (select slimPar64_1 universe) 0     -< e
       f2 <- flame (select ultrabar_1 universe) (pi/2) -< e
       f3 <- flame (select hex12p1 universe)    (pi)   -< e
       f4 <- flame (select gb_par_1 (select gb_1 universe)) (3*pi/2)   -< e
       f5 <- flame (select gb_par_2 (select gb_1 universe)) (3*pi/2)   -< e
--        :+: select ultrabar_1 universe :+: select hex12p1 universe :+: select gb_par_1 (select gb_1 universe) :+: select gb_par_2 (select gb_1 universe)
       d1  <- pure (setParams ((derbyCon DerbyConRed) :+: (derbyRotation (DerbyClockwise 5))) (select gb_derby_1 (select gb_1 universe))) -< e
       d2  <- pure (setParams ((derbyCon DerbyConRed) :+: (derbyRotation (DerbyCounterClockwise 5))) (select gb_derby_2 (select gb_1 universe))) -< e
       m <- masters -< e
       returnA -< mergeParamsL [m, f1, f2, f3, f4, f5, d1, d2]


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
--     for dur . pure (sp GBLaserRedGreenStrobe lzr) -->
--     for dur . pure (sp GBLaserRedStrobeGreen lzr) -->
--     for dur . pure (sp GBLaserRedGreenAlternate lzr) -->
--     for dur . pure (sp GBLaserBlackout lzr) -->
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
--          s <- laserStrobe lzr -< e
          p <- laserPattern lzr -< e
          returnA -< mergeParamsL [c,p]

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
