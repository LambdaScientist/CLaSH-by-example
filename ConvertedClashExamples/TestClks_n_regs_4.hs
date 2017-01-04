{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures   #-}
{-# LANGUAGE MagicHash        #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE RankNTypes  #-}
{-# LANGUAGE ScopedTypeVariables  #-}

module TestClks_n_regs_4 where

import qualified Prelude as P
import CLaSH.Prelude

import SAFE.TestingTools
import SAFE.CommonClash

import ClocksAndRegisters.Clks_n_regs_4

import Text.PrettyPrint.HughesPJClass

instance Pretty PIn where
  pPrint PIn {..} = text "PIn:"
                $+$ text "_clk ="   <+> showT _clk
                $+$ text "_reset =" <+> showT _reset
                $+$ text "_start =" <+> showT _start
                $+$ text "_stop ="  <+> showT _stop
instance Pretty St where
 pPrint St {..} = text "St"
              $+$ text "_cnt_en ="   <+>  showT _cnt_en
              $+$ text "_count_us =" <+>  showT _count_us
              $+$ text "_stop_d1 ="  <+>  showT _stop_d1
              $+$ text "_stop_d2 ="  <+>  showT _stop_d2
              $+$ text "_count ="    <+>  showT _count
configurationList :: [Config]
configurationList = [configOne, configTwo, configThree, configFour]
  where
    startSt    = St False 0 False False 0

    inputOne = PIn 0 False False False
    configOne = Config inputOne startSt

    inputTwo  = PIn 0 False False False
    configTwo = Config inputTwo startSt

    inputThree  = PIn 0 False False False
    configThree = Config inputThree startSt

    inputFour  = PIn 0 False False False
    configFour = Config inputFour startSt
    
---TESTING
instance PortIn PIn
instance SysState St

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
