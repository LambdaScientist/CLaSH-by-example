{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures   #-}
{-# LANGUAGE MagicHash        #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TemplateHaskell  #-}


module BusSignals where

import qualified Prelude as P
import CLaSH.Prelude
import Control.Lens hiding ((:>))
import Control.Monad.Trans.State
--inputs
data PIn = PIn { _in_1 :: BitVector 4
               , _in_2 :: BitVector 4
               , _in_3 :: Bit
               } deriving (Eq)
instance Show PIn where
  show PIn {..} =
         "PIn\n\t _in_1 = " P.++ show _in_1
    P.++ "\n\t _in_2 = " P.++ show _in_2
    P.++ "\n\t _in_3 = " P.++ show _in_3
--Outputs and state data
data St = St { _out_1 :: BitVector 4
             } deriving (Eq)
makeLenses ''St
instance Show St where
 show St {..} =
        "St\n\t _out_1 = " P.++ show _out_1

procSimple :: St -> PIn -> St
procSimple st@St{..} PIn{..} = flip execState st $
  out_1 .= keepValue (bnot _in_3) _in_1 .|. keepValue _in_3 _in_2
  where
    keepValue keepDecider =  v2bv.map (.&. keepDecider).bv2v

bnot :: Bit -> Bit
bnot 1 = 0
bnot _ = 1

bit2Bool :: Bit -> Bool
bit2Bool 1 = True
bit2Bool _ = False

topEntity :: St -> Signal PIn -> Signal St
topEntity st pin = reg
  where
    reg = register st (procSimple <$> reg <*> pin)

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

    startSt    = St 0

    inputOne   = PIn 1 1 1
    configOne  = Config inputOne startSt
    testOne    = runOneTest configOne

    inputTwo    = PIn 1 1 0
    configTwo  = Config inputTwo startSt
    testTwo    = runOneTest configTwo

    inputThree = PIn 1 0 1
    configThree  = Config inputThree startSt
    testThree  = runOneTest configThree

    inputFour  = PIn 0 1 1
    configFour  = Config inputFour startSt
    testFour   = runOneTest configFour
