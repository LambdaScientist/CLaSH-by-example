{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module StateMachines.Models.StateMachine2 where

import CLaSH.Prelude

import Control.Lens hiding ((:>))
import Control.Monad.Trans.State
import Control.Monad

import SAFE.TestingTools
import SAFE.CommonClash

import Text.PrettyPrint.HughesPJClass

import Control.DeepSeq
import GHC.Generics (Generic)

--inputs
data PIn = PIn { _clk   :: Bit
               , _reset :: ResetStatus
               , _go    :: GoStatus
               , _kill  :: KillStatus
               } deriving (Eq, Show, Generic, NFData)

--Outputs and state data
data StateLabel = Idle | Active | Finish | Abort deriving (Show, Eq, Generic, NFData)
data DoneStatus     = Done | NotDone deriving (Eq, Show, Generic, NFData)
data SignalStatus   = IsRising | NotRising deriving (Eq, Show, Generic, NFData)
data KillStatus     = Terminate | DontKill deriving (Eq, Show, Generic, NFData)
data GoStatus       = Go | DontGo deriving (Eq, Show, Generic, NFData)
data ResetStatus    = ResetEnabled | ResetDisabled deriving (Eq, Show, Generic, NFData)

getSignalStatus :: (Bounded a, Eq a) => a -> Signal a -> Signal SignalStatus
getSignalStatus value sigValue = status <$> isRising value sigValue
  where
    status rising = if rising then IsRising else NotRising

data St = St { _stateReg :: StateLabel
             , _count     :: BitVector 8
             , _done      :: DoneStatus
             } deriving (Eq, Show, Generic, NFData)
makeLenses ''St

reset :: St
reset = St Idle 0 NotDone

onRun :: St -> PIn -> SignalStatus -> St
onRun _ PIn{_reset = ResetEnabled} _ = reset
onRun st@St{..} pin@PIn{..} IsRising = stateAction
  where
    stateAction :: St
    stateAction | _stateReg == Idle && _go == Go = (setCountTo0.setDoneToDone) st{_stateReg = Active}
                | _stateReg == Active && _kill == Terminate = (incCount.setDoneToNotDone) st{_stateReg = Finish}
                | _stateReg == Active && _count == 64 = (incCount.setDoneToNotDone) st{_stateReg = Finish}
                | _stateReg == Active = (incCount.setDoneToNotDone) st
                | _stateReg == Finish = (setCountTo0.setDoneToDone) st{_stateReg = Idle}
                | _stateReg == Abort && _kill == Terminate = (setCountTo0.setDoneToNotDone) st{_stateReg = Idle}
                | otherwise  = (setCountTo0.setDoneToNotDone) st
    setCountTo0 = (count .~ 0)
    setDoneToDone = (done .~ Done)
    setDoneToNotDone = (done .~ NotDone)
    incCount = (count +~ 1)
onRun st _ _ = st


topEntity :: Signal PIn -> Signal St
topEntity = topEntity' st
  where
    st = St Idle 0 NotDone

topEntity' :: St ->  Signal PIn -> Signal St--Signal st
topEntity' st pin = result
  where
    result = register st (onRun <$> result <*> pin <*> rising )
    rising = getSignalStatus 0 clk
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
               $+$ text "_stateReg =" <+> showT _stateReg
               $+$ text "_count ="     <+> showT _count
               $+$ text "_done ="      <+> showT _done
