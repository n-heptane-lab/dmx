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
    (DerbyConGreenBlue)    -> Param $ 150
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

instance ToParam GBParConVal where
  type ToParameter GBParConVal = GBParCon
  toParam = parCon

instance ToParam GBLaserColorVal where
  type ToParameter GBLaserColorVal = GBLaserColor
  toParam = gbLaserColor
