{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures   #-}
{-# LANGUAGE MagicHash        #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TemplateHaskell  #-}


module SimpleDFlopWithReset where

import qualified Prelude as P
import CLaSH.Prelude
import Control.Lens hiding ((:>))
import Control.Monad.Trans.State
import CLaSH.Sized.Internal.BitVector

import CLaSH.Signal.Delayed.Explicit


--inputs
data PIn = PIn { _in_1  :: Bit
               , _clk   :: Bit
               , _reset :: Bool
               } deriving (Show, Eq)
--Outputs and state data
data St = St { _out_1 :: Bit
             } deriving (Show, Eq)
makeLenses ''St



onTrue :: St -> PIn -> Bool -> St
onTrue st PIn{..} condition = if _reset then St 0 else doinstead
  where
    doinstead = if condition then st{ _out_1 = _in_1 } else st

bnot :: Bit -> Bit
bnot 1 = 0
bnot _ = 1


topEntity :: St -> Signal PIn -> Signal St
topEntity st pin = result
  where
    result = register st (onTrue <$> result <*> pin <*> rising )
    rising = isRising 0 clk
    clk = _clk <$> pin

runTop :: Signal St
runTop = topEntity (St 0 ) input
  where
    input = PIn  <$> oscillate <*> oscillate <*> reset
    oscillate = register 1 (bnot <$> oscillate)
    reset = signal False --
-- bnot :: Bit -> Bit
-- bnot 1 = 0
-- bnot _ = 1
