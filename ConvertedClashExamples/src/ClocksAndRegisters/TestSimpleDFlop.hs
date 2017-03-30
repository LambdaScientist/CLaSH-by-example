{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards  #-}

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module ClocksAndRegisters.TestSimpleDFlop where

import CLaSH.Prelude

import SAFE.TestingTools
import SAFE.CommonClash

import ClocksAndRegisters.Models.SimpleDFlop

import Text.PrettyPrint.HughesPJClass
import GHC.Generics (Generic)
import Control.DeepSeq



configurationList :: [Config]
configurationList = [configOne, configTwo, configThree, configFour]
  where
    startSt    = St 0

    inputOne  = signal $ PIn 0 0
    configOne = Config inputOne startSt

    inputTwo  = signal $ PIn 0 1
    configTwo = Config inputTwo startSt

    inputThree  = signal $ PIn 1 0
    configThree = Config inputThree startSt

    startStOther    = St 1
    inputFour  = signal $ PIn 1 1
    configFour = Config inputFour startStOther

---TESTING
data Config = Config { input  :: Signal PIn
                     , startSt :: St
                     }
instance NFData Config where
  rnf a = seq a ()

instance Pretty Config where
  pPrint Config{..} = text "Config:"
                  -- $+$ text "input ="   <+> pPrint input
                  $+$ text "startSt =" <+> pPrint startSt
instance Transition Config where
  runOneTest = runOneTest'
setupTest :: Config -> Signal St
setupTest (Config pin st) = topEntity' st pin

setupAndRun :: [[TestResult]]
setupAndRun = runConfigList' False 5 setupTest configurationList

ppSetupAndRun :: Doc
ppSetupAndRun = pPrint setupAndRun

ppSetupAndRun2 :: String
ppSetupAndRun2 = show setupAndRun
