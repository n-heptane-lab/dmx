{-# language DataKinds #-}
module Hex12p where

import Core

-- * Hex12p

type Hex12p_7channel = '[Red, Green, Blue, White, Amber, UV, Master]

hex12p :: Address -> Fixture Hex12p_7channel
hex12p addr = Fixture { address = addr }


