{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE NoImplicitPrelude  #-}

module InAndOut.Models.IntermediateSignal where

import CLaSH.Prelude
import Control.Lens hiding ((:>))
import Control.Monad.Trans.State

import Text.PrettyPrint.HughesPJClass

import SAFE.TestingTools
import SAFE.CommonClash

--inputs
data PIn = PIn { _in1 :: Bit
               , _in2 :: Bit
               , _in3 :: Bit
               } deriving (Eq, Show)

--Outputs and state data
data St = St { _out1 :: Bit
             , _out2 :: Bit
             } deriving (Eq, Show)
makeLenses ''St

procSimple :: St -> PIn -> St
procSimple st@St{..} PIn{..} = flip execState st $ do
  out1 .= ( intermediate_sig .&. _in3)
  out2 .= ( intermediate_sig .|. _in3)
  where
    intermediate_sig = _in1 .&. _in2

topEntity :: Signal PIn -> Signal St
topEntity = topEntity' st
  where
    st = St 0 0

topEntity' :: St ->  Signal PIn -> Signal St--Signal st
topEntity' st pin = reg
  where
    reg = register st $ procSimple <$> reg <*> pin


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
              $+$ text "_cntEn ="   <+>  showT _out1
              $+$ text "_cntEn ="   <+>  showT _out2
