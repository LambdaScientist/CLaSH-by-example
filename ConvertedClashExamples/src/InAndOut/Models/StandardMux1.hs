{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE NoImplicitPrelude  #-}

module InAndOut.Models.StandardMux1 where

import CLaSH.Prelude
import Control.Lens hiding ((:>))
import Control.Monad.Trans.State

import SAFE.TestingTools
import SAFE.CommonClash

import Text.PrettyPrint.HughesPJClass

--inputs
data PIn = PIn { _in1 :: BitVector 4
               , _in2 :: BitVector 4
               , _in3 :: Bit
               } deriving (Eq, Show)

--Outputs and state data
data St = St { _out1 :: BitVector 4
             } deriving (Eq, Show)
makeLenses ''St

procSimple :: St -> PIn -> St
procSimple st@St{..} PIn{..} = st & (out1 .~ multiPlex _in3 _in1 _in2)

multiPlex :: Bit -> BitVector 4 -> BitVector 4 -> BitVector 4
multiPlex 1 _ y = y
multiPlex 0 x _ = x
multiPlex _ _ _ = 0

topEntity :: Signal PIn -> Signal St
topEntity = topEntity' st
  where
    st = St 0

topEntity' :: St ->  Signal PIn -> Signal St--Signal st
topEntity' st pin = reg
  where
    reg = register st (procSimple <$> reg <*> pin)


--- The following code is only for a custom testing framework, and PrettyPrinted  output
instance PortIn PIn
instance Pretty PIn where
  pPrint PIn {..} = text "PIn:"
                $+$ text "_in1 =" <+> showT _in1
                $+$ text "_in2 =" <+> showT _in2
                $+$ text "_in3 =" <+> showT _in3

instance SysState St
instance Pretty St where
  pPrint St {..} = text "St"
               $+$ text "_out1 ="   <+>  showT _out1
