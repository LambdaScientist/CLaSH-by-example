{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE NoImplicitPrelude  #-}

module InAndOut.Models.SimpleInOut where

import CLaSH.Prelude
import Control.Lens hiding ((:>))
import Control.Monad.Trans.State

import SAFE.TestingTools
import SAFE.CommonClash

import Text.PrettyPrint.HughesPJClass

import GHC.Generics (Generic)
import Control.DeepSeq


--inputs
data PIn = PIn { _in1 :: Bit
               , _in2 :: Bit
               , _in3 :: Bit
               } deriving (Eq, Show)
instance NFData PIn where
  rnf a = seq a ()
--Outputs and state data
data St = St { _out1 :: Bit
             , _out2 :: Bit
             } deriving (Eq, Show)
makeLenses ''St
instance NFData St where
  rnf a = seq a ()

procSimple :: St -> PIn -> St
procSimple st@St{..} PIn{..} = st & setOutput1.setOutput2
  where
    setOutput1 = out1 .~ (_in1 .&. _in2 .&. _in3)
    setOutput2 = out2 .~ (_in1 .|. _in2 .|. _in3)

topEntity :: Signal PIn -> Signal St
topEntity = topEntity' st
  where
    st = St 0 0

topEntity' :: St ->  Signal PIn -> Signal St
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
              $+$ text "_out1 =" <+>  showT _out1
              $+$ text "_out2 =" <+>  showT _out2
