module ModB where

import           CLaSH.Prelude
import CLaSH.Sized.Internal.Unsigned
import qualified ModA
import Language.Haskell.TH

type CustomType = Unsigned $( litT $ numTyLit $ toInteger  $ ModA.f ModA.xyz  )

foo :: CustomType
foo = 5
foo2 :: Unsigned $( litT $ numTyLit $ toInteger  $ ModA.f ModA.xyz )
foo2 = foo

baz :: Unsigned $( litT $ numTyLit $ toInteger  $ ModA.f ModA.xyz  )
baz = 6
baz2 :: CustomType
baz2 = baz



topEntity :: a -> (CustomType, Unsigned $( litT $ numTyLit $ toInteger  $ ModA.f ModA.xyz  ))
topEntity  x = (foo, baz)
