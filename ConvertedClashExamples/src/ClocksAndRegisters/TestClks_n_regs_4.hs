{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards  #-}

module ClocksAndRegisters.TestClks_n_regs_4 where

import CLaSH.Prelude

import SAFE.TestingTools
import SAFE.CommonClash

import ClocksAndRegisters.Models.Clks_n_regs_4

import Text.PrettyPrint.HughesPJClass

configurationList :: [Config]
configurationList = [configOne, configTwo, configThree, configFour]
  where
    startSt    = St CounterDisabled 0 StopDisabled StopDisabled 0

    inputOne = PIn 0 ResetDisabled StartDisabled StopDisabled
    configOne = Config inputOne startSt

    inputTwo  = PIn 0 ResetDisabled StartDisabled StopDisabled
    configTwo = Config inputTwo startSt

    inputThree  = PIn 0 ResetDisabled StartDisabled StopDisabled
    configThree = Config inputThree startSt

    inputFour  = PIn 0 ResetDisabled StartDisabled StopDisabled
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
