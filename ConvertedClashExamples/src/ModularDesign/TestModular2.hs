{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards  #-}

module ModularDesign.TestModular2 where

import CLaSH.Prelude

import SAFE.TestingTools
import SAFE.CommonClash

import ModularDesign.Models.Modular2

import Text.PrettyPrint.HughesPJClass

import GHC.Generics (Generic)
import Control.DeepSeq

import qualified ModularDesign.Models.StateMachine as SM

configurationList :: [Config]
configurationList = [configOne, configTwo, configThree, configFour]
  where
    startSt    = St SM.Active 99 False False False False

    inputOne   = MPIn 1 True False (PPIn True False) (PPIn True True) (PPIn False True)
    configOne  = Config inputOne startSt

    inputTwo    = MPIn 1 False False (PPIn True False) (PPIn True False) (PPIn False True)
    configTwo  = Config inputTwo startSt

    inputThree = MPIn 1 False True (PPIn True False) (PPIn True False) (PPIn True False)
    configThree  = Config inputThree startSt

    inputFour  = MPIn 1 False False (PPIn True False) (PPIn True False) (PPIn True False)
    configFour  = Config inputFour startSt

---TESTING
data Config = Config { input  :: MPIn
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
