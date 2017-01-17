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

--Outputs and state data
data St = St { _out1 :: Bit
             } deriving (Eq, Show)
makeLenses ''St

onTrue :: St -> PIn -> Bool -> St
onTrue st PIn{..} edgeDetect = shouldReset
  where
    shouldReset = if _reset then St 0 else risingEdge
    risingEdge = if edgeDetect then enabled else st
    enabled = if _enable then shouldClear  else st
    shouldClear = if _clearN then St 0 else st{ _out1 = _in1 }

topEntity :: Signal PIn -> Signal St
topEntity = topEntity' st
  where
    st = St 0

topEntity' :: St -> Signal PIn -> Signal St
topEntity' st pin = result
  where
    result = register st (onTrue <$> result <*> pin <*> rising )
    rising = isRising 0 clk
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
