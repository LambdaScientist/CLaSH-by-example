{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures   #-}
{-# LANGUAGE MagicHash        #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TemplateHaskell  #-}


module StateMachine where

import qualified Prelude as P
import CLaSH.Prelude
import Control.Lens hiding ((:>))
import Control.Monad.Trans.State
import Control.Monad
import CLaSH.Sized.Internal.BitVector

-- import CLaSH.Signal.Delayed.Explicit


--inputs
data PIn = PIn { _clk   :: Bit
               , _reset :: Bool
               , _go    :: Bool
               , _kill  :: Bool
               } deriving (Show, Eq)
--Outputs and state data
data StateLabel = Idle | Active | Finish | Abort deriving (Show, Eq)

data St = St { _state_reg :: StateLabel
             , _count     :: BitVector 8
             , _done      :: Bool
             } deriving (Show, Eq)
makeLenses ''St



reset :: St
reset = St Idle 0 False


onRun :: St -> PIn -> Bool -> St
onRun st@St{..} PIn{..} risingEdge = flip execState st $ do
  if _reset then put $ reset
  else
    case _state_reg of
      Idle   -> when _go $ state_reg .= Active
      Active -> if _kill then state_reg .= Abort else when (_count == 64)  $ state_reg .= Finish
      Finish -> state_reg .= Idle
      Abort  -> unless _kill $ state_reg .= Idle
      otherwise  -> state_reg .= Idle
  when risingEdge $ do
    if (_state_reg == Finish || _state_reg == Abort) then count .= 0
                                                  else when (_state_reg == Active) $ count += 1
    if (_state_reg == Finish) then done .= True else done .= False
