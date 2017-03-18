{-# language DataKinds #-}
module Ultrabar where

import Core

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
