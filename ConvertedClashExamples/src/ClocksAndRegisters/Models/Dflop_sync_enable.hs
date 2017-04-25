{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE NoImplicitPrelude  #-}

module ClocksAndRegisters.Models.Dflop_sync_enable where

import CLaSH.Prelude

import Control.Lens hiding ((:>))

import Text.PrettyPrint.HughesPJClass
import GHC.Generics (Generic)
import Control.DeepSeq

import SAFE.TestingTools
import SAFE.CommonClash

--inputs
data PIn = PIn { _clk    :: Bit
               , _in1     :: Bit
               , _reset   :: Bool
               , _enable  :: Bool
               , _clearN :: Bool
               } deriving (Eq, Show)
instance NFData PIn where
  rnf a = seq a ()
--Outputs and state data
data St = St { _out1 :: Bit
             } deriving (Eq, Show)
makeLenses ''St
instance NFData St where
  rnf a = seq a ()

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
instance PortIn PIn
instance Pretty PIn where
  pPrint PIn {..} = text "PIn:"
                $+$ text "_clk ="    <+> showT _clk
                $+$ text "_in1 ="    <+> showT _in1
                $+$ text "_reset ="  <+> showT _reset
                $+$ text "_enable =" <+> showT _enable
                $+$ text "_clearN ="  <+> showT _clearN

instance SysState St
instance Pretty St where
  pPrint St {..} = text "St"
               $+$ text "_out1 ="   <+>  showT _out1
