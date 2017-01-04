{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures   #-}
{-# LANGUAGE MagicHash        #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TemplateHaskell  #-}

module TestSimpleDFlop where

import qualified Prelude as P
import CLaSH.Prelude

import SAFE.TestingTools
import SAFE.CommonClash

import ClocksAndRegisters.SimpleDFlop

import Text.PrettyPrint.HughesPJClass

instance Pretty PIn where
  pPrint PIn {..} = text "PIn:"
                $+$ text "_in_1 =" <+> showT _in_1
                $+$ text "_clk ="  <+> showT _clk
instance Pretty St where
 pPrint St {..} = text "St"
              $+$ text "_out_1 =" <+> showT _out_1

configurationList :: [Config]
configurationList = [configOne, configTwo, configThree, configFour]
  where
    startSt    = St 0

    inputOne  = PIn 0 0
    configOne = Config inputOne startSt

    inputTwo  = PIn 0 0
    configTwo = Config inputTwo startSt

    inputThree  = PIn 0 0
    configThree = Config inputThree startSt

    inputFour  = PIn 0 0
    configFour = Config inputFour startSt

---TESTING
instance PortIn PIn
instance SysState St

data Config = Config { input  :: PIn
                     , startSt :: St
                     }
instance Pretty Config where
  pPrint Config{..} = text "Config:"
                  $+$ text "input ="   <+> pPrint input
                  $+$ text "startSt =" <+> pPrint startSt
instance Transition Config where
  runOneTest = runOneTest'

setupTest :: Config -> Signal St
setupTest (Config pin st) = topEntity' st sPin
  where
    sPin = signal pin

setupAndRun :: [[TestResult]]
setupAndRun = runConfigList setupTest configurationList
