{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE NoImplicitPrelude  #-}

module ClocksAndRegisters.Models.Clks_n_regs_4 where

import CLaSH.Prelude

import Control.Lens hiding ((:>))
import Control.Monad.Trans.State
import Control.Monad

import SAFE.TestingTools
import SAFE.CommonClash

import Text.PrettyPrint.HughesPJClass

data SignalStatus  = IsRising | NotRising deriving (Eq, Show)
data CounterStatus = CounterEnabled | CounterDisabled deriving (Eq, Show)
data StartStatus   = StartEnabled | StartDisabled deriving (Eq, Show)
data StopStatus    = StopEnabled | StopDisabled deriving (Eq, Show)
data ResetStatus   = ResetEnabled | ResetDisabled deriving (Eq, Show)
data CounterLimitStatus  = CntLimitReached | CntLimitNotReached deriving (Eq, Show)

--inputs
data PIn = PIn { _clk   :: Bit
               , _reset :: ResetStatus
               , _start :: StartStatus
               , _stop  :: StopStatus
               } deriving (Eq, Show)

--Outputs and state data
data St = St { _cntEn   :: CounterStatus
             , _countUs :: BitVector 4
             , _stopD1  :: StopStatus
             , _stopD2  :: StopStatus
             , _count   :: BitVector 4
             } deriving (Eq, Show)
makeLenses ''St

resetSTWithCount :: BitVector 4 -> St
resetSTWithCount = St CounterDisabled 0 StopDisabled StopDisabled

convertBool :: (Bounded a, Eq a) => a -> Signal a -> Signal SignalStatus
convertBool value sigValue = status <$> isRising value sigValue
  where
    status rising = if rising then IsRising else NotRising

onTrue :: St -> PIn -> SignalStatus -> St
onTrue St{_count = curCount}    PIn{_reset = ResetEnabled} _ = resetSTWithCount curCount
onTrue st                       _                  NotRising = st
onTrue st@St{..}                PIn{..}            IsRising  = procState st
  where
    procState = risingState.(stateChange _start _stop _cntEn getCounterLimitStatus)
    stateChange :: StartStatus -> StopStatus -> CounterStatus -> CounterLimitStatus -> (St -> St)
    stateChange StartEnabled _ CounterEnabled CntLimitReached   = resetCount.enableCounter
    stateChange StartEnabled _ _ CntLimitNotReached             = enableCounter
    stateChange _ StopEnabled CounterEnabled CntLimitReached    = resetCount.disableCounter
    stateChange _ StopEnabled _ CntLimitReached                 = disableCounter
    stateChange _ StopEnabled CounterEnabled CntLimitNotReached = incCount.disableCounter
    stateChange _ StopEnabled _ CntLimitNotReached              = disableCounter
    stateChange _ _ _ _= id

    getCounterLimitStatus :: CounterLimitStatus
    getCounterLimitStatus = if _count == 13 then CntLimitReached else CntLimitNotReached

    risingState    = (stopD1 .~ _stop) . (stopD2 .~ _stopD1)
    disableCounter = cntEn .~ CounterDisabled
    enableCounter  = cntEn .~ CounterEnabled
    resetCount     = countUs .~ 0
    incCount       = countUs +~ 1
onTrue st _ _ = st

topEntity :: Signal PIn -> Signal St
topEntity = topEntity' st
  where
    st = St CounterDisabled 0 StopDisabled StopDisabled 0

topEntity' :: St ->  Signal PIn -> Signal St--Signal st
topEntity' st pin = result
  where
    result = register st (onTrue <$> result <*> pin <*> rising )
    rising = convertBool 0 clk
    clk = _clk <$> pin


--- The following code is only for a custom testing framework, and PrettyPrinted  output

instance PortIn PIn
instance Pretty PIn where
  pPrint PIn {..} = text "PIn:"
                $+$ text "_clk ="   <+> showT _clk
                $+$ text "_reset =" <+> showT _reset
                $+$ text "_start =" <+> showT _start
                $+$ text "_stop ="  <+> showT _stop

instance SysState St
instance Pretty St where
  pPrint St {..} = text "St"
               $+$ text "_cntEn ="   <+>  showT _cntEn
               $+$ text "_countUs =" <+>  showT _countUs
               $+$ text "_stopD1 ="  <+>  showT _stopD1
               $+$ text "_stopD2 ="  <+>  showT _stopD2
               $+$ text "_count ="    <+>  showT _count
