{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE NoImplicitPrelude  #-}

module ClocksAndRegisters.Models.SimpleDFlopWithReset where

import CLaSH.Prelude

import Control.Lens hiding ((:>))

import Text.PrettyPrint.HughesPJClass

import SAFE.TestingTools
import SAFE.CommonClash

data SignalStatus  = IsRising | NotRising deriving (Eq, Show)
data ResetStatus   = ResetEnabled | ResetDisabled deriving (Eq, Show)

--inputs
data PIn = PIn { _in1   :: Bit
               , _clk   :: Bit
               , _reset :: ResetStatus
               } deriving (Eq, Show)

data St = St { _out1 :: Bit
             } deriving (Eq, Show)
makeLenses ''St

onTrue :: St -> PIn -> SignalStatus -> St
onTrue st PIn{_reset = ResetEnabled} _ = St 0
onTrue st PIn{_in1 = input1} IsRising = st{ _out1 = input1 }
onTrue st _ _ = st

getSignalStatus :: (Bounded a, Eq a) => a -> Signal a -> Signal SignalStatus
getSignalStatus value sigValue = status <$> isRising value sigValue
  where
    status rising = if rising then IsRising else NotRising

topEntity :: Signal PIn -> Signal St
topEntity = topEntity' startSt
  where
    startSt = St 0

topEntity' :: St -> Signal PIn -> Signal St
topEntity' st pin = result
  where
    result = register st (onTrue <$> result <*> pin <*> rising )
    rising = getSignalStatus 0 clk
    clk = _clk <$> pin


--- The following code is only for a custom testing framework, and PrettyPrinted  output

instance PortIn PIn
instance Pretty PIn where
  pPrint PIn {..} = text "PIn:"
                $+$ text "_in1 ="  <+> showT _in1
                $+$ text "_clk ="   <+> showT _clk
                $+$ text "_reset =" <+> showT _reset

instance SysState St
instance Pretty St where
  pPrint St {..} = text "St"
               $+$ text "_out1 =" <+> showT _out1
