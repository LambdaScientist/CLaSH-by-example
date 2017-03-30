{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE NoImplicitPrelude  #-}

module ClocksAndRegisters.Models.Dflop_sync_enable where

import CLaSH.Prelude

import Control.Lens hiding ((:>))

import Text.PrettyPrint.HughesPJClass

import SAFE.TestingTools
import SAFE.CommonClash


import GHC.Generics (Generic)
import Control.DeepSeq



data SignalStatus  = IsRising | NotRising deriving (Eq, Show)
data ActiveStatus  = Enabled | Disabled deriving (Eq, Show)
data ResetStatus   = ResetEnabled | ResetDisabled deriving (Eq, Show)
data ClearStatus   = ClearEnabled | ClearDisabled deriving (Eq, Show)

--inputs
data PIn = PIn { _in1    :: Bit
               , _clk    :: Bit
               , _reset  :: ResetStatus
               , _enable :: ActiveStatus
               , _clearN :: ClearStatus
               } deriving (Eq, Show)
instance NFData PIn where
  rnf a = seq a ()
--Outputs and state data
data St = St { _out1 :: Bit
             } deriving (Eq, Show)
makeLenses ''St
instance NFData St where
  rnf a = seq a () 

getSignalStatus :: (Bounded a, Eq a) => a -> Signal a -> Signal SignalStatus
getSignalStatus value sigValue = status <$> isRising value sigValue
  where
    status rising = if rising then IsRising else NotRising

onTrue :: St -> PIn -> SignalStatus -> St
onTrue st PIn{_reset = ResetEnabled} _ = St 0
onTrue st PIn{_enable = Enabled, _clearN = ClearEnabled} IsRising = St 0
onTrue st PIn{_enable = Enabled, _in1 = input1} IsRising = st{ _out1 = input1 }
onTrue st _ _ = st


topEntity :: Signal PIn -> Signal St
topEntity = topEntity' st
  where
    st = St 0

topEntity' :: St -> Signal PIn -> Signal St
topEntity' st pin = result
  where
    result = register st (onTrue <$> result <*> pin <*> rising )
    rising = getSignalStatus 0 clk
    clk = _clk <$> pin

--- The following code is only for a custom testing framework, and PrettyPrinted  output
instance SysState St
instance Pretty St where
  pPrint St {..} = text "St"
               $+$ text "_out1 ="   <+>  showT _out1

instance PortIn PIn
instance Pretty PIn where
  pPrint PIn {..} = text "PIn:"
                $+$ text "_in1 ="    <+> showT _in1
                $+$ text "_clk ="    <+> showT _clk
                $+$ text "_reset ="  <+> showT _reset
                $+$ text "_enable =" <+> showT _enable
                $+$ text "_clearN =" <+> showT _clearN
