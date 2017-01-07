{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures   #-}
{-# LANGUAGE MagicHash        #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TemplateHaskell  #-}


module SimpleInOut where

import qualified Prelude as P
import CLaSH.Prelude
import Control.Lens hiding ((:>))
import Control.Monad.Trans.State

--inputs
data PIn = PIn { _in_1 :: Bit
               , _in_2 :: Bit
               , _in_3 :: Bit
               } deriving ( Eq)
instance Show PIn where
  show PIn {..} =
      "PIn\n\t _in_1 = " P.++ show _in_1
    P.++ "\n\t _in_2 = " P.++ show _in_2
    P.++ "\n\t _in_3 = " P.++ show _in_3
--Outputs and state data
data St = St { _out_1 :: Bit
             , _out_2 :: Bit
             } deriving (Eq)
makeLenses ''St
instance Show St where
 show St {..} =
        "St\n\t _out_1 = " P.++ show _out_1
   P.++ "\n\t _in_2 = " P.++ show _out_2


procSimple :: St -> PIn -> St
procSimple st@St{..} PIn{..} = flip execState st $ do
  out_1 .= (_in_1 .&. _in_2 .&. _in_3)
  out_2 .= (_in_1 .|. _in_2 .|. _in_3)

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

configList :: [Config]
configList = [configOne, configTwo, configThree, configFour]
  where
    startSt    = St 0 0

    inputOne   = PIn 1 1 1
    configOne  = Config inputOne startSt

    inputTwo    = PIn 1 1 0
    configTwo  = Config inputTwo startSt

    inputThree = PIn 1 0 1
    configThree  = Config inputThree startSt

    inputFour  = PIn 0 1 1
    configFour  = Config inputFour startSt

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
