{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures   #-}
{-# LANGUAGE MagicHash        #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TemplateHaskell  #-}


module Modular1 where

import qualified Prelude as P
import CLaSH.Prelude
import Control.Lens hiding ((:>))
import Control.Monad.Trans.State
import Control.Monad
import CLaSH.Sized.Internal.BitVector

import qualified StateMachine as SM

-- import CLaSH.Signal.Delayed.Explicit


--inputs
-- data PIn = PIn { _clk   :: Bit
--                , _reset :: Bool
--                , _go    :: Bool
--                , _kill  :: Bool
--                } deriving (Eq)
-- instance Show PIn where
--   show PIn {..} =
--     "PIn\n\t _clk = " P.++ show _clk
--     P.++ "\n\t _reset = " P.++ show _reset
--     P.++ "\n\t _go = " P.++ show _go
--     P.++ "\n\t _kill = " P.++ show _kill

data Partial = PPIn { _go'    :: Bool
                    , _kill'  :: Bool
                    } deriving (Eq)
instance Show Partial where
  show PPIn {..} =
    "PPIn\n\t _go' = " P.++ show _go'
    P.++ "\n\t _kill' = " P.++ show _kill'

data MPIn = MPIn { _clk'     :: Bit
                 , _reset'   :: Bool
                 , _kill_clr :: Bool
                 , _pin1     :: Partial
                 , _pin2     :: Partial
                 , _pin3     :: Partial
                 } deriving (Eq)
instance Show MPIn where
  show MPIn {..} =
    "PPIn\n\t _clk' = " P.++ show _clk'
    P.++ "\n\t _reset' = " P.++ show _reset'
    P.++ "\n\t  _kill_clr = " P.++ show _kill_clr
    P.++ "\n\t  _pin1 = " P.++ show _pin1
    P.++ "\n\t  _pin2 = " P.++ show _pin2
    P.++ "\n\t  _pin3 = " P.++ show _pin3
--Outputs and state data
partial2full (PPIn go kill) clk reset = SM.PIn clk reset go kill

data StateLabel = Idle | Active | Finish | Abort deriving (Show, Eq)

data St = St { _state_reg :: SM.StateLabel
             , _count     :: BitVector 8
             , _done1      :: Bool
             , _done2      :: Bool
             , _done3      :: Bool
             , _kill_ltchd :: Bool
             } deriving (Eq)
makeLenses ''St
instance Show St where
 show St {..} =
        "St\n\t _state_reg = " P.++ show _state_reg
   P.++ "\n\t _count = " P.++ show _count
   P.++ "\n\t _done1 = " P.++ show _done1
   P.++ "\n\t _done2 = " P.++ show _done2
   P.++ "\n\t _done3 = " P.++ show _done3
   P.++ "\n\t _kill_ltchd = " P.++ show _kill_ltchd


resetSt :: St -> St
resetSt (St x y z1 z2 z3  _) = St x y z1 z2 z3 False

partial = PPIn False False

runRegProc :: St -> MPIn -> Bool -> St
runRegProc st@St{..} MPIn{..} rising = flip execState st $
  if _reset' then put $ resetSt st
  else when rising $
    if kill_1 || kill_2 || kill_3 then kill_ltchd .= True
      else when _kill_clr $ kill_ltchd .= False
  where
    kill_1 = _kill' _pin1
    kill_2 = _kill' _pin2
    kill_3 = _kill' _pin3
--
--
-- runSt :: St -> PIn -> Bool -> St
-- runSt st@St{..} PIn{..} risingEdge = flip execState st $ do
--   if _reset then put $ resetSt st
--   else
--     case _state_reg of
--       Idle   -> when _go $ state_reg .= Active
--       Active -> if _kill then state_reg .= Abort else when (_count == 64)  $ state_reg .= Finish
--       Finish -> state_reg .= Idle
--       Abort  -> unless _kill $ state_reg .= Idle
--       _  -> state_reg .= Idle
--   when risingEdge $ do
--     if _state_reg == Finish || _state_reg == Abort then count .= 0
--                                                    else when (_state_reg == Active) $ count += 1
--     if _state_reg == Finish then done .= True else done .= False

setStDone:: St -> Bool -> Bool -> Bool -> St
setStDone st d1 d2 d3 = st {_done1 = d1, _done2 = d2, _done3 = d3}

topEntity :: St -> Signal MPIn -> Signal St
topEntity st mp = setStDone <$> pin  <*> (SM._done <$> s1) <*> (SM._done <$> s2) <*> (SM._done <$> s3)
  where
    startSt = SM.St SM.Idle 0 False
    pin = register (St SM.Idle 0 False False False False) (runRegProc <$> pin <*> mp <*> signal False)
    sbool = signal False
    pin1 = _pin1 <$> mp
    pin2 = _pin2 <$> mp
    pin3 = _pin3 <$> mp
    clk = _clk' <$> mp
    reset = _reset' <$> mp
    s1 = register startSt  (SM.onRun <$> s1 <*> (partial2full <$>  pin1 <*>  clk <*> reset) <*> sbool)
    s2 = register startSt  (SM.onRun <$> s2 <*> (partial2full <$>  pin2 <*>  clk <*> reset) <*> sbool)
    s3 = register startSt  (SM.onRun <$> s3 <*> (partial2full <$>  pin3 <*>  clk <*> reset) <*> sbool)




--
---TESTING
data TestResult = TestResult { initConfig  :: Config
                             , endSt        :: St
                             }deriving (Eq)
instance Show TestResult where
  show TestResult {..} =
         "TestResult:\n initConfig = " P.++ show initConfig
    P.++ "\n Result = " P.++ show endSt
    P.++ "\n\n"
data Config = Config { input  :: MPIn
                     , startS :: St
                     }deriving (Eq)
instance Show Config where
 show Config {..} =
        "Config:\n input = " P.++ show input
   P.++ "\n startS = " P.++ show startS


runOneTest :: Config -> Signal TestResult
runOneTest config = TestResult config <$> result
  where
    result = topEntity startingState inputSignal
    startingState = resetSt (St SM.Idle 0 False False False False)
    inputSignal   = signal $ input config

runAllTests :: [(TestResult,TestResult,TestResult,TestResult)]
runAllTests = getTestResults True 2

getTestResults ::  Bool -> Int ->  [(TestResult,TestResult,TestResult,TestResult)]
getTestResults getTail howManyResults= conTail.sampleN howManyResults  $ bundle (testOne, testTwo, testThree, testFour)
  where
    conTail x = if getTail then P.tail x else x

    startSt    = St SM.Idle 0 False False False False

    inputOne   = MPIn 0 False False (PPIn False False) (PPIn False False) (PPIn False False)
    configOne  = Config inputOne startSt
    testOne    = runOneTest configOne

    inputTwo    = MPIn 0 False False (PPIn False False) (PPIn False False) (PPIn False False)
    configTwo  = Config inputTwo startSt
    testTwo    = runOneTest configTwo

    inputThree = MPIn 0 False False (PPIn False False) (PPIn False False) (PPIn False False)
    configThree  = Config inputThree startSt
    testThree  = runOneTest configThree

    inputFour  = MPIn 0 False False (PPIn False False) (PPIn False False) (PPIn False False)
    configFour  = Config inputFour startSt
    testFour   = runOneTest configFour
