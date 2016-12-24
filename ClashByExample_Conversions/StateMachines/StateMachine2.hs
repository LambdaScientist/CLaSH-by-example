{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures   #-}
{-# LANGUAGE MagicHash        #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TemplateHaskell  #-}


module StateMachine2 where

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
               } deriving (Eq)
--
instance Show PIn where
  show PIn {..} =
    "PIn\n\t _clk = " P.++ show _clk
    P.++ "\n\t _reset = " P.++ show _reset
    P.++ "\n\t _go = " P.++ show _go
    P.++ "\n\t _kill = " P.++ show _kill

--Outputs and state data
data StateLabel = Idle | Active | Finish | Abort deriving (Show, Eq)

data St = St { _state_reg :: StateLabel
             , _count     :: BitVector 8
             , _done      :: Bool
             } deriving (Eq)
makeLenses ''St
instance Show St where
 show St {..} =
        "St\n\t _state_reg = " P.++ show _state_reg
   P.++ "\n\t _count = " P.++ show _count
   P.++ "\n\t _done = " P.++ show _done



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
                     else when _count == 64  $ state_reg .= Finish
        Finish -> do count .= 0
                     done .= True
                     state_reg .= Idle
        Abort  -> do count .= 0
                     done .= False
                     unless _kill $ state_reg .= Idle
        otherwise  -> put reset

topEntity :: St -> Signal PIn -> Signal St
topEntity st pin = result
  where
    result = register st (onRun <$> result <*> pin <*> rising )
    rising = isRising 0 clk
    clk = _clk <$> pin



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
data Config = Config { input  :: PIn
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
    startingState = startS config
    inputSignal   = signal $ input config

runAllTests :: [(TestResult,TestResult,TestResult,TestResult)]
runAllTests = getTestResults True 2

getTestResults ::  Bool -> Int ->  [(TestResult,TestResult,TestResult,TestResult)]
getTestResults getTail howManyResults= conTail.sampleN howManyResults  $ bundle (testOne, testTwo, testThree, testFour)
  where
    conTail x = if getTail then P.tail x else x

    startSt    = St Idle 0 False

    inputOne   = PIn 1 False False False
    configOne  = Config inputOne startSt
    testOne    = runOneTest configOne

    inputTwo    = PIn 1 False False False
    configTwo  = Config inputTwo startSt
    testTwo    = runOneTest configTwo

    inputThree = PIn 1 False False False
    configThree  = Config inputThree startSt
    testThree  = runOneTest configThree

    inputFour  = PIn 1 False False False
    configFour  = Config inputFour startSt
    testFour   = runOneTest configFour
