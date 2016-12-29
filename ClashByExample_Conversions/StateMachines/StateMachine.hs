{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures   #-}
{-# LANGUAGE MagicHash        #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TemplateHaskell  #-}


module StateMachine where

import qualified Prelude as P
import CLaSH.Prelude hiding (otherwise)
import Control.Lens hiding ((:>))
import Control.Monad.Trans.State
import Control.Monad


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

topEntity :: St -> Signal PIn -> Signal St
topEntity st pin = result
  where
    result = register st (onRun <$> result <*> pin <*> rising )
    rising = isRising 0 clk
    clk = _clk <$> pin




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


configList :: [Config]
configList = [configOne, configTwo, configThree, configFour]
  where
    startSt = St Idle 0 False

    inputOne  = PIn 1 False False False
    configOne = Config inputOne startSt

    inputTwo  = PIn 1 False False False
    configTwo = Config inputTwo startSt

    inputThree  = PIn 1 False False False
    configThree = Config inputThree startSt

    inputFour  = PIn 1 False False False
    configFour = Config inputFour startSt

getTestResult ::  Bool -> Int -> Config ->  [TestResult]
getTestResult getTail howManyResults config = conTail $ sampleN howManyResults test
  where
    conTail x = if getTail then P.tail x else x
    test      = runOneTest config

runConfigList :: [Config] -> [[TestResult]]
runConfigList = runConfigList' True 2

runConfigList' :: Bool -> Int -> [Config] -> [[TestResult]]
runConfigList' getTail howMany = P.map test
  where
    test = getTestResult getTail howMany

defaultTest :: [[TestResult]]
defaultTest = runConfigList configList
