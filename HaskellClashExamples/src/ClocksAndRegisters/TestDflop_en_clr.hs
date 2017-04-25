{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards  #-}

module ClocksAndRegisters.TestDflop_en_clr where

import CLaSH.Prelude

import SAFE.TestingTools
import SAFE.CommonClash

import ClocksAndRegisters.Models.Dflop_en_clr

import Text.PrettyPrint.HughesPJClass

import GHC.Generics (Generic)
import Control.DeepSeq

configurationList :: [Config]
configurationList = [configOne, configTwo, configThree, configFour]
  where
    startSt    = St 0

    inputOne  = PIn 1 1 ResetEnabled Disabled ClearDisabled
    configOne = Config inputOne startSt

    inputTwo  = PIn 0 1 ResetDisabled Enabled ClearEnabled
    configTwo = Config inputTwo startSt

    inputThree  = PIn 1 1 ResetDisabled Enabled ClearDisabled
    configThree = Config inputThree startSt

    inputFour  = PIn 1 0 ResetDisabled Disabled ClearEnabled
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
