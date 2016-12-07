{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures   #-}
{-# LANGUAGE MagicHash        #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TemplateHaskell  #-}


module Dflop_en_clr where

import qualified Prelude as P
import CLaSH.Prelude
import Control.Lens hiding ((:>))
import Control.Monad.Trans.State
import CLaSH.Sized.Internal.BitVector

import CLaSH.Signal.Delayed.Explicit


--inputs
data PIn = PIn { _in_1    :: Bit
               , _clk     :: Bit
               , _reset   :: Bool
               , _enable  :: Bool
               , _clear_n :: Bool
               } deriving (Show, Eq)
--Outputs and state data
data St = St { _out_1 :: Bit
             } deriving (Show, Eq)
makeLenses ''St



onTrue :: St -> PIn -> Bool -> St
onTrue st PIn{..} edgeDetect = shouldReset
  where
    shouldReset = if _reset then St 0 else risingEdge
    risingEdge = if edgeDetect then shouldClear else st
    shouldClear = if _clear_n then St 0 else enabled
    enabled = if _enable then st{ _out_1 = _in_1 }  else st

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
    input = PIn  <$> oscillate <*> oscillate <*> reset <*> enable <*> clear
    oscillate = register 1 (bnot <$> oscillate)
    reset = signal False --
    clear = signal False --
    enable = signal False --
-- bnot :: Bit -> Bit
-- bnot 1 = 0
-- bnot _ = 1
