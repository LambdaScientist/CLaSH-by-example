{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards  #-}

module StateMachines.TestStateMachine2 where

import CLaSH.Prelude

import SAFE.TestingTools
import SAFE.CommonClash

import StateMachines.Models.StateMachine2

import Text.PrettyPrint.HughesPJClass

import GHC.Generics (Generic)
import Control.DeepSeq


configurationList :: [Config]
configurationList = [configOne, configTwo, configThree, configFour]
  where
    startSt = St Active 99 False

    inputOne  = PIn 1 True False False
    configOne = Config inputOne startSt

    inputTwo  = PIn 1 False True True
    configTwo = Config inputTwo startSt

    inputThree  = PIn 1 False True False
    configThree = Config inputThree startSt

    inputFour  = PIn 1 False False False
    configFour = Config inputFour startSt

---TESTING

instance NFData Config where
  rnf a = seq a ()
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
