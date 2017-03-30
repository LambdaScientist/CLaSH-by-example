{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards  #-}

module InAndOut.TestBusBreakout where

import CLaSH.Prelude

import SAFE.TestingTools
import SAFE.CommonClash

import InAndOut.Models.BusBreakout

import Text.PrettyPrint.HughesPJClass

configurationList :: [Config]
configurationList = [configOne, configTwo, configThree, configFour]
  where
    startSt    = St 0

    inputOne   = PIn 0 0 1
    configOne  = Config inputOne startSt

    inputTwo    = PIn 1 0 1
    configTwo  = Config inputTwo startSt

    inputThree = PIn 1 1 0
    configThree  = Config inputThree startSt

    inputFour  = PIn 1 1 1
    configFour  = Config inputFour startSt
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
