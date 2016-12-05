{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures   #-}
{-# LANGUAGE MagicHash        #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TemplateHaskell  #-}


module Clks_n_regs_4 where

import qualified Prelude as P
import CLaSH.Prelude
import Control.Lens hiding ((:>))
import Control.Monad.Trans.State
import Control.Monad
import CLaSH.Sized.Internal.BitVector

import CLaSH.Signal.Delayed.Explicit


--inputs
data PIn = PIn { _clk   :: Bit
               , _reset :: Bool
               , _start  :: Bool
               , _stop   :: Bool
               } deriving (Show, Eq)
--Outputs and state data
data St = St { _cnt_en   :: Bool
             , _count_us :: BitVector 4
             , _stop_d1  :: Bool
             , _stop_d2  :: Bool
             , _count    :: BitVector 4
             } deriving (Show, Eq)
makeLenses ''St

resetSTKeepCount :: BitVector 4 -> St
resetSTKeepCount = St False 0 False False


onTrue :: St -> PIn -> Bool -> St
onTrue st@St{..} PIn{..} risingEdge = flip execState st $
  if _reset then put $ resetSTKeepCount _count
  else
    when risingEdge $ do
      --SR Flop
      if _start then  cnt_en .= True
      else when _stop $ cnt_en .= False
      --Counter
      when _cnt_en $ if _count_us == (13::BitVector 4) then count_us .= 0
                    else count_us += 1
      stop_d1 .= _stop
      stop_d2 .= _stop_d1

bnot :: Bit -> Bit
bnot 1 = 0
bnot _ = 1

-- bit2Bool :: Bit -> Bool
-- bit2Bool 1 = True
-- bit2Bool _ = False

-- topEntity :: St -> Signal PIn -> Signal St
-- topEntity st pin = result
--   where
--     result = register st (onTrue <$> result <*> pin <*> rising )
--     rising = isRising 0 clk
--     clk = _clk <$> pin

-- runTop :: Signal St
-- runTop = topEntity (St 0 ) input
--   where
--     input = PIn  <$> oscillate <*> oscillate <*> reset
--     oscillate = register 1 (bnot <$> oscillate)
--     reset = signal False --
