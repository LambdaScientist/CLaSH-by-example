{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE NoImplicitPrelude  #-}

module ClocksAndRegisters.Models.Clks_n_regs_4 where

import CLaSH.Prelude

import Control.Lens hiding ((:>))
import Control.Monad.Trans.State
import Control.Monad

import SAFE.TestingTools
import SAFE.CommonClash

import Text.PrettyPrint.HughesPJClass

--inputs
data PIn = PIn { _clk   :: Bit
               , _reset :: Bool
               , _start :: Bool
               , _stop  :: Bool
               } deriving (Eq, Show)

--Outputs and state data
data St = St { _cntEn   :: Bool
             , _countUs :: BitVector 4
             , _stopD1  :: Bool
             , _stopD2  :: Bool
             , _count    :: BitVector 4
             } deriving (Eq, Show)
makeLenses ''St

resetSTWithCount :: BitVector 4 -> St
resetSTWithCount = St False 0 False False

onTrue :: St -> PIn -> Bool -> St
onTrue st@St{..} PIn{..} risingEdge = flip execState st $
  if _reset then put $ resetSTWithCount _count
  else
    when risingEdge $ do
      --SR Flop
      if _start then  cntEn .= True
      else when _stop $ cntEn .= False
      --Counter
      when _cntEn $ if _countUs == (13::BitVector 4) then countUs .= 0
                    else countUs += 1
      stopD1 .= _stop
      stopD2 .= _stopD1

topEntity :: Signal PIn -> Signal St
topEntity = topEntity' st
  where
    st = St False 0 False False 0

topEntity' :: St ->  Signal PIn -> Signal St--Signal st
topEntity' st pin = result
  where
    result = register st (onTrue <$> result <*> pin <*> rising )
    rising = isRising 0 clk
    clk = _clk <$> pin


--- The following code is only for a custom testing framework, and PrettyPrinted  output

instance PortIn PIn
instance Pretty PIn where
  pPrint PIn {..} = text "PIn:"
                $+$ text "_clk ="   <+> showT _clk
                $+$ text "_reset =" <+> showT _reset
                $+$ text "_start =" <+> showT _start
                $+$ text "_stop ="  <+> showT _stop

instance SysState St
instance Pretty St where
  pPrint St {..} = text "St"
               $+$ text "_cntEn ="   <+>  showT _cntEn
               $+$ text "_countUs =" <+>  showT _countUs
               $+$ text "_stopD1 ="  <+>  showT _stopD1
               $+$ text "_stopD2 ="  <+>  showT _stopD2
               $+$ text "_count ="    <+>  showT _count
