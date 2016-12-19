{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}

module ModA where

import           CLaSH.Prelude
import CLaSH.Sized.Internal.Unsigned

-----------------------------------------------------------------------------------------------------------------------------------

f :: (Floating a, RealFrac a) => a ->  Int
f x = floor x
xyz::(Floating a, RealFrac a) => a
--xyz = logBase 2 11
xyz = foo/bar
foo :: RealFrac a => a
foo = 72.0
bar :: RealFrac a => a
bar = 9.0
