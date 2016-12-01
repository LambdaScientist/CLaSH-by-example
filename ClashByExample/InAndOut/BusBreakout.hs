{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures   #-}
{-# LANGUAGE MagicHash        #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TemplateHaskell  #-}


module BusBreakout where

import qualified Prelude as P
import CLaSH.Prelude
import Control.Lens hiding ((:>))
import Control.Monad.Trans.State
import CLaSH.Sized.Internal.BitVector

--inputs
data PIn = PIn { _in_1 :: BitVector 4
               , _in_2 :: BitVector 4
               , _in_3 :: Bit
               } deriving (Show, Eq)
--Outputs and state data
data St = St { _out_1 :: BitVector 6
             } deriving (Show, Eq)
makeLenses ''St


procSimple :: St -> PIn -> St
procSimple st@St{..} PIn{..} = flip execState st $
  out_1 .= (p1 ++# p2 ++# p3 ++# p4)
  where
    p1 = slice d3 d2 _in_2
    p2 = _in_1 ! 3 .&. _in_2 ! 1
    p3 = _in_1 ! 2 .&. _in_2 ! 0
    p4 = slice d1 d0 _in_1
-- Also Works
-- procSimple2 :: St -> PIn -> St
-- procSimple2 st@St{..} PIn{..} = flip execState st $
--   out_1 .= slice d3 d2 _in_2
--        ++# (_in_1 ! 3 .&. _in_2 ! 1)
--        ++# (_in_1 ! 2 .&. _in_2 ! 0)
--        ++# (slice d1 d0 _in_1)


bnot :: Bit -> Bit
bnot 1 = 0
bnot _ = 1


bit2Bool :: Bit -> Bool
bit2Bool 1 = True
bit2Bool _ = False

topEntity :: St -> Signal PIn -> Signal St
topEntity st pin = reg
  where
    reg = register st (procSimple <$> reg <*> pin)

runTop :: Signal St
runTop = topEntity (St 0 ) (signal $ PIn 1 0 1)
