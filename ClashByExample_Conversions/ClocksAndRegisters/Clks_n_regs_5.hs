{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures   #-}
{-# LANGUAGE MagicHash        #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TemplateHaskell  #-}


module Clks_n_regs_5 where

import qualified Prelude as P
import CLaSH.Prelude
import Control.Lens hiding ((:>))
import Control.Monad.Trans.State
import Control.Monad

-- import CLaSH.Signal.Delayed.Explicit


--inputs
data PIn = PIn { _clk   :: Bit
               , _reset :: Bool
               , _start  :: Bool
               , _stop   :: Bool
               } deriving (Eq)
instance Show PIn where
  show PIn {..} =
    "PIn\n\t _clk = " P.++ show _clk
    P.++ "\n\t _reset = " P.++ show _reset
    P.++ "\n\t _start = " P.++ show _start
    P.++ "\n\t _stop = " P.++ show _stop
--Outputs and state data
data St = St { _cnt_en   :: Bool
             , _count_us :: BitVector 4
             , _stop_d1  :: Bool
             , _stop_d2  :: Bool
             , _count    :: BitVector 4
             } deriving (Eq)
makeLenses ''St
instance Show St where
 show St {..} =
        "St\n\t _cnt_en = " P.++ show _cnt_en
   P.++ "\n\t _count_us = " P.++ show _count_us
   P.++ "\n\t _stop_d1 = " P.++ show _stop_d1
   P.++ "\n\t _stop_d2 = " P.++ show _stop_d2
   P.++ "\n\t _count = " P.++ show _count


resetSTKeepCount :: BitVector 4 -> St
resetSTKeepCount = St False 0 False False




onTrue :: St -> PIn -> Bool -> St
onTrue st@St{..} PIn{..} risingEdge = flip execState st $
  if _reset then put $ resetSTKeepCount _count
  else
    when risingEdge $ do
      --SR Flop
      if _start then  cnt_en .= True
      else when _stop $ cnt_en .= False
      --Counter
      varCntEn <- use cnt_en --this gets the current value of the
      when varCntEn $ if _count_us == (13::BitVector 4) then count_us .= 0
                    else count_us += 1
      stop_d1 .= _stop
      stop_d2 .= _stop_d1

bnot :: Bit -> Bit
bnot 1 = 0
bnot _ = 1

topEntity :: St -> Signal PIn -> Signal St
topEntity st pin = result
  where
    result = register st (onTrue <$> result <*> pin <*> rising )
    rising = isRising 0 clk
    clk = _clk <$> pin

-- bit2Bool :: Bit -> Bool
-- bit2Bool 1 = True
-- bit2Bool _ = False

---TESTING

runTop :: Signal St
runTop = topEntity (St False 0 False False 0 ) input
  where
    input = PIn  <$> oscillate <*> reset <*> oscillateTF <*> oscillateTF
    oscillate = register 1 (bnot <$> oscillate)
    reset = signal False --
    oscillateTF = register True (not <$> oscillateTF)


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

    startSt    = St False 0 False False 0

    inputOne  = PIn 0 False False False
    configOne = Config inputOne startSt
    testOne   = runOneTest configOne

    inputTwo  = PIn 0 False False False
    configTwo = Config inputTwo startSt
    testTwo   = runOneTest configTwo

    inputThree  = PIn 0 False False False
    configThree = Config inputThree startSt
    testThree   = runOneTest configThree

    inputFour  = PIn 0 False False False
    configFour = Config inputFour startSt
    testFour   = runOneTest configFour
