{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module StateMachines.Models.StateMachine where

import CLaSH.Prelude

import Control.Lens hiding ((:>))
import Control.Monad.Trans.State
import Control.Monad

import SAFE.TestingTools
import SAFE.CommonClash

import Text.PrettyPrint.HughesPJClass

import Control.DeepSeq
import GHC.Generics (Generic)

data DoneStatus     = Done | NotDone deriving (Eq, Show, Generic, NFData)
data SignalStatus   = IsRising | NotRising deriving (Eq, Show, Generic, NFData)
data KillStatus     = Terminate | DontKill deriving (Eq, Show, Generic, NFData)
data GoStatus       = Go | DontGo deriving (Eq, Show, Generic, NFData)
data ResetStatus    = ResetEnabled | ResetDisabled deriving (Eq, Show, Generic, NFData)
data CounterLimitStatus  = CntLimitReached | CntLimitNotReached deriving (Eq, Show, Generic, NFData)

getSignalStatus :: (Bounded a, Eq a) => a -> Signal a -> Signal SignalStatus
getSignalStatus value sigValue = status <$> isRising value sigValue
  where
    status rising = if rising then IsRising else NotRising

--inputs
data PIn = PIn { _clk   :: Bit
               , _reset :: ResetStatus
               , _go    :: GoStatus
               , _kill  :: KillStatus
               } deriving (Eq, Show, Generic, NFData)

--Outputs and state data
data StateLabel = Idle | Active | Finish | Abort deriving (Show, Eq, Generic, NFData)

data St = St { _stateReg :: StateLabel
             , _count     :: BitVector 8
             , _done      :: DoneStatus
             } deriving (Eq, Show, Generic, NFData)
makeLenses ''St

nextState :: St -> PIn -> SignalStatus -> St
nextState st PIn{_reset = ResetEnabled} _                           = st{_stateReg = Idle}
nextState st@St{_stateReg = Idle} PIn{_go = Go} IsRising            = st{_stateReg = Active}
nextState st@St{_stateReg = Idle} _ IsRising                        = st
nextState st@St{_stateReg = Active} PIn{_kill = Terminate} IsRising = st{_stateReg = Abort}
nextState st@St{_stateReg = Active, _count = 100} _ IsRising         = st{_stateReg = Finish}
nextState st@St{_stateReg = Finish} _ IsRising                      = st{_stateReg = Idle}
nextState st _ _                                                    = st

stateActions :: St -> PIn -> SignalStatus -> (St->St)
stateActions _ PIn{_reset = ResetEnabled} _ = (count .~ 0).(done .~ NotDone)
stateActions st@St{..} pin@PIn{..} IsRising = stateAction
  where
    stateAction :: (St -> St)
    stateAction | _stateReg == Finish = setCountTo0.setDoneToDone
                | _stateReg == Abort  = setCountTo0.setDoneToNotDone
                | _stateReg == Active = incCount.setDoneToNotDone 
                | otherwise = id
    setCountTo0 = (count .~ 0)
    setDoneToDone = (done .~ Done)
    setDoneToNotDone = (done .~ NotDone)
    incCount = (count +~ 1)
stateActions st _ _ = id



onRun :: St -> PIn -> SignalStatus -> St
onRun st pin ss = stateActions st pin ss $ nextState st pin ss

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
