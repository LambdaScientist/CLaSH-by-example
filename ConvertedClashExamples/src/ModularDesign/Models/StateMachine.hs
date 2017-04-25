{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards  #-}

module ModularDesign.Models.StateMachine where

import CLaSH.Prelude

import Control.Lens hiding ((:>))
import Control.Monad.Trans.State
import Control.Monad

import SAFE.TestingTools
import SAFE.CommonClash

import GHC.Generics (Generic)
import Control.DeepSeq

import Text.PrettyPrint.HughesPJClass

--inputs
data PIn = PIn { _clk   :: Bit
               , _reset :: Bool
               , _go    :: Bool
               , _kill  :: Bool
               } deriving (Eq, Show)

instance NFData PIn where
  rnf a = seq a ()

--Outputs and state data
data StateLabel = Idle | Active | Finish | Abort deriving (Show, Eq)

data St = St { _state_reg :: StateLabel
             , _count     :: BitVector 8
             , _done      :: Bool
             } deriving (Eq, Show)
makeLenses ''St
instance NFData St where
  rnf a = seq a ()

reset :: St
reset = St Idle 0 False

onRun :: St -> PIn -> Bool -> St
onRun st@St{..} PIn{..} risingEdge = flip execState st $ do
  if _reset then put reset
  else do 
    case _state_reg of
      Idle   -> when _go $ state_reg .= Active
      Active -> if _kill then state_reg .= Abort
                else when (_count == 64)  $ state_reg .= Finish
      Finish -> state_reg .= Idle
      Abort  -> unless _kill $ state_reg .= Idle
      _  -> state_reg .= Idle
    when risingEdge $ do
      if _state_reg == Finish || _state_reg == Abort then count .= 0
                                                      else when (_state_reg == Active) $ count += 1
      if _state_reg == Finish then done .= True else done .= False

topEntity :: Signal PIn -> Signal St
topEntity = topEntity' st
  where
    st = St Idle 0 False

topEntity' :: St ->  Signal PIn -> Signal St--Signal st
topEntity' st pin = result
  where
    result = register st (onRun <$> result <*> pin <*> rising )
    rising = isRising 0 clk
    clk = _clk <$> pin


--- The following code is only for a custom testing framework, and PrettyPrinted  output
instance PortIn PIn
instance Pretty PIn where
  pPrint PIn {..} = text "PIn:"
                $+$ text "_clk ="   <+> showT _clk
                $+$ text "_reset =" <+> showT _reset
                $+$ text "_go ="    <+> showT _go
                $+$ text "_kill ="  <+> showT _kill

instance SysState St
instance Pretty St where
  pPrint St {..} = text "St:"
               $+$ text "_state_reg =" <+> showT _state_reg
               $+$ text "_count ="     <+> showT _count
               $+$ text "_done ="      <+> showT _done
