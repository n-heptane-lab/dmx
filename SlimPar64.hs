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

