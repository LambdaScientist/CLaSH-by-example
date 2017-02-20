{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards  #-}

module ClocksAndRegisters.TestSimpleDFlopWithReset where

import CLaSH.Prelude

import SAFE.TestingTools
import SAFE.CommonClash

import ClocksAndRegisters.Models.SimpleDFlopWithReset

import Text.PrettyPrint.HughesPJClass
import GHC.Generics (Generic)
import Control.DeepSeq


configurationList :: [Config]
configurationList = [configOne, configTwo, configThree, configFour]
  where
    startSt    = St 0

    inputOne  = PIn 0 0 False
    configOne = Config inputOne startSt

    inputTwo  = PIn 0 0 False
    configTwo = Config inputTwo startSt

    inputThree  = PIn 0 0 False
    configThree = Config inputThree startSt

    inputFour  = PIn 0 0 False
    configFour = Config inputFour startSt

---TESTING

foo n | x == 2 = 2
      | x== 3 = 3
      | x == 4 = 4
      | bar n == 5 = 5
  where
    x = bar n
foo n = case bar n of
        2 -> bar n == 3 = 3
      | bar n == 4 = 4
      | bar n == 5 = 5
data Config = Config { input  :: PIn
                     , startSt :: St
                     }
instance Pretty Config where
  pPrint Config{..} = text "Config:"
                  $+$ text "input ="   <+> pPrint input
                  $+$ text "startSt =" <+> pPrint startSt
instance Transition Config where
  runOneTest = runOneTest'
instance NFData Config where
  rnf a = seq a ()

setupTest :: Config -> Signal St
setupTest (Config pin st) = topEntity' st sPin
  where
    sPin = signal pin

setupAndRun :: [[TestResult]]
setupAndRun = runConfigList setupTest configurationList

ppSetupAndRun :: Doc
ppSetupAndRun = pPrint setupAndRun
