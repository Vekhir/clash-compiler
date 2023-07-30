{-|
  Copyright   :  (C) 2019, Google Inc.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  Christiaan Baaij <christiaan.baaij@gmail.com>
-}
{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -Wno-orphans #-}
-- {-# OPTIONS_GHC -ddump-splices #-}

module Clash.Class.AutoReg.Instances where

import           Clash.Class.AutoReg.Internal
import           Clash.CPP                           (maxTupleSize)

import           Data.Complex (Complex)
import           Data.Ord (Down)
import           Data.Ratio (Ratio)

deriveAutoReg ''Complex
deriveAutoReg ''Down
deriveAutoReg ''Ratio

-- | __NB__: The documentation only shows instances up to /3/-tuples. By
-- default, instances up to and including /12/-tuples will exist. If the flag
-- @large-tuples@ is set instances up to the GHC imposed limit will exist. The
-- GHC imposed limit is either 62 or 64 depending on the GHC version.
deriveAutoRegTuples [2..maxTupleSize]
