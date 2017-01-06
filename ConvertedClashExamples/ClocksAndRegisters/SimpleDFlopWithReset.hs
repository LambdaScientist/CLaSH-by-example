{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE NoImplicitPrelude  #-}

module ClocksAndRegisters.SimpleDFlopWithReset where

import CLaSH.Prelude

import Control.Lens hiding ((:>))

import Text.PrettyPrint.HughesPJClass

import SAFE.TestingTools
import SAFE.CommonClash

--inputs
data PIn = PIn { _in1 :: Bit
               , _clk  :: Bit
               , _reset :: Bool
               } deriving (Eq, Show)
instance PortIn PIn
instance Pretty PIn where
  pPrint PIn {..} = text "PIn:"
                $+$ text "_in1 ="  <+> showT _in1
                $+$ text "_clk ="   <+> showT _clk
                $+$ text "_reset =" <+> showT _reset

data St = St { _out1 :: Bit
             } deriving (Eq, Show)
makeLenses ''St
instance SysState St
instance Pretty St where
 pPrint St {..} = text "St"
              $+$ text "_out1 =" <+> showT _out1

onTrue :: St -> PIn -> Bool -> St
onTrue st PIn{..} condition = if _reset then St 0 else doinstead
  where
    doinstead = if condition then st{ _out1 = _in1 } else st

bnot :: Bit -> Bit
bnot 1 = 0
bnot _ = 1

topEntity :: Signal PIn -> Signal St
topEntity = topEntity' startSt
  where
    startSt = St 0

topEntity' :: St -> Signal PIn -> Signal St
topEntity' st pin = result
  where
    result = register st (onTrue <$> result <*> pin <*> rising )
    rising = isRising 0 clk
    clk = _clk <$> pin

---TESTING
