{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards  #-}

module StateMachines.Models.StateMachine2 where

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
               , _go    :: Bool
               , _kill  :: Bool
               } deriving (Eq, Show)
instance PortIn PIn
instance Pretty PIn where
  pPrint PIn {..} = text "PIn:"
                $+$ text "_clk ="   <+> showT _clk
                $+$ text "_reset =" <+> showT _reset
                $+$ text "_go ="    <+> showT _go
                $+$ text "_kill ="  <+> showT _kill

--Outputs and state data
data StateLabel = Idle | Active | Finish | Abort deriving (Show, Eq)

data St = St { _state_reg :: StateLabel
             , _count     :: BitVector 8
             , _done      :: Bool
             } deriving (Eq, Show)
makeLenses ''St
instance SysState St
instance Pretty St where
  pPrint St {..} = text "St:"
               $+$ text "_state_reg =" <+> showT _state_reg
               $+$ text "_count ="     <+> showT _count
               $+$ text "_done ="      <+> showT _done

reset :: St
reset = St Idle 0 False

onRun :: St -> PIn -> Bool -> St
onRun st@St{..} PIn{..} risingEdge = flip execState st $ do
  if _reset then put reset
  else
    when risingEdge $ do
      case _state_reg of
        Idle   -> when _go $ do state_reg .= Active
                                count .= 0
                                done  .= False
        Active -> do count += 1
                     done .= False
                     if _kill then state_reg .= Abort
                     else when (_count == 64)  $ state_reg .= Finish
        Finish -> do count .= 0
                     done .= True
                     state_reg .= Idle
        Abort  -> do count .= 0
                     done .= False
                     unless _kill $ state_reg .= Idle
        _      -> put reset

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



--
---TESTING
