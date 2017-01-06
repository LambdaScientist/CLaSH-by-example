{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards  #-}

module TestClks_n_regs_4 where

import CLaSH.Prelude

import SAFE.TestingTools
import SAFE.CommonClash

import InAndOut.BusBreakout

import Text.PrettyPrint.HughesPJClass

configurationList :: [Config]
configurationList = [configOne, configTwo, configThree, configFour]
  where
    startSt    = St 0

    inputOne   = PIn 1 1 1
    configOne  = Config inputOne startSt

    inputTwo    = PIn 1 1 0
    configTwo  = Config inputTwo startSt

    inputThree = PIn 1 0 1
    configThree  = Config inputThree startSt

    inputFour  = PIn 0 1 1
    configFour  = Config inputFour startSt
---TESTING

-- Move these when I seperate the files
instance PortIn PIn
instance SysState St
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