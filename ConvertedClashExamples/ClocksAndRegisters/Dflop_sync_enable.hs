{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE NoImplicitPrelude  #-}

module ClocksAndRegisters.Dflop_en_clr where

  import CLaSH.Prelude

  import Control.Lens hiding ((:>))
  import Control.Monad.Trans.State
  import Control.Monad

  import Text.PrettyPrint.HughesPJClass

  --------------------------------------------------------------------------------
  -- Remove this when test is turned into a library
  --------------------------------------------------------------------------------

  showT :: (Show s) => s -> Doc
  showT = text.show

  --------------------------------------------------------------------------------

--inputs
data PIn = PIn { _in_1    :: Bit
               , _clk     :: Bit
               , _reset   :: Bool
               , _enable  :: Bool
               , _clear_n :: Bool
               } deriving (Eq)
instance Show PIn where
  show PIn {..} =
    "PIn\n\t _in_1 = " P.++ show _in_1
    P.++ "\n\t _clk = " P.++ show _clk
    P.++ "\n\t _reset = " P.++ show _reset
    P.++ "\n\t _enable = " P.++ show _enable
    P.++ "\n\t _clear_n = " P.++ show _clear_n
--Outputs and state data
data St = St { _out_1 :: Bit
             } deriving (Eq)
makeLenses ''St
instance Show St where
 show St {..} =
        "St\n\t _out_1 = " P.++ show _out_1

onTrue :: St -> PIn -> Bool -> St
onTrue st PIn{..} edgeDetect = shouldReset
  where
    shouldReset = if _reset then St 0 else risingEdge
    risingEdge = if edgeDetect then enabled else st
    enabled = if _enable then shouldClear  else st
    shouldClear = if _clear_n then St 0 else st{ _out_1 = _in_1 }

bnot :: Bit -> Bit
bnot 1 = 0
bnot _ = 1

topEntity :: St -> Signal PIn -> Signal St
topEntity st pin = result
  where
    result = register st (onTrue <$> result <*> pin <*> rising )
    rising = isRising 0 clk
    clk = _clk <$> pin
    ---TESTING

runTop :: Signal St
runTop = topEntity (St 0 ) input
  where
    input = PIn  <$> oscillate <*> oscillate <*> reset <*> enable <*> clear
    oscillate = register 1 (bnot <$> oscillate)
    reset = signal False --
    clear = signal False --
    enable = signal False --


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
    startSt    = St 0

    inputOne  = PIn 0 0 False False False
    configOne = Config inputOne startSt

    inputTwo  = PIn 0 0 False False False
    configTwo = Config inputTwo startSt

    inputThree  = PIn 0 0 False False False
    configThree = Config inputThree startSt

    inputFour  = PIn 0 0 False False False
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
