{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards  #-}

module StateMachines.TestStateMachine where

import CLaSH.Prelude

import SAFE.TestingTools
import SAFE.CommonClash

import StateMachines.Models.StateMachine

import Text.PrettyPrint.HughesPJClass

configurationList :: [Config]
configurationList = [configOne, configTwo, configThree, configFour]
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

---TESTING


----------------------------------------

data Config = Config { input  :: PIn
                     , startSt :: St
                     }deriving(Eq,Show)
instance Pretty Config where
 pPrint Config{..} = text "Config:"
                 $+$ text "input ="   <+> pPrint input
                 $+$ text "startSt =" <+>  pPrint startSt
instance  Transition Config where
  runOneTest = runOneTest'

setupTest :: Config -> Signal St
setupTest (Config pin st) = topEntity' st sPin
  where
    sPin = signal pin

setupAndRun :: [[TestResult]]
setupAndRun = runConfigList setupTest configurationList

ppSetupAndRun :: Doc
ppSetupAndRun = pPrint setupAndRun
