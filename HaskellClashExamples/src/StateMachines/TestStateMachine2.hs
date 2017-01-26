{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module StateMachines.TestStateMachine2 where

import CLaSH.Prelude

import SAFE.TestingTools
import SAFE.CommonClash

import StateMachines.Models.StateMachine2

import Text.PrettyPrint.HughesPJClass

import Control.DeepSeq
import GHC.Generics (Generic)

configurationList :: [Config]
configurationList = [configOne, configTwo, configThree, configFour]
  where
    startSt = St Idle 0 NotDone

    inputOne  = PIn 1 ResetEnabled DontGo Terminate
    configOne = Config inputOne startSt

    inputTwo  = PIn 1 ResetEnabled DontGo Terminate
    configTwo = Config inputTwo startSt

    inputThree  = PIn 1 ResetEnabled DontGo Terminate
    configThree = Config inputThree startSt

    inputFour  = PIn 1 ResetEnabled DontGo Terminate
    configFour = Config inputFour startSt


---TESTING

data Config = Config { input  :: PIn
                     , startSt :: St
                     }deriving(Eq,Show, Generic, NFData)
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
