{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE RankNTypes  #-}
{-# LANGUAGE ExistentialQuantification  #-}

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module SAFE.TestingTools where

import qualified Prelude as P
import CLaSH.Prelude

import Text.PrettyPrint.HughesPJClass

import GHC.Generics (Generic)
import Control.DeepSeq

--------------------------------------------------------------------------------
-- Abstract Area
--------------------------------------------------------------------------------
class (Eq pin, Pretty pin) => PortIn pin

class (Eq st, Pretty st) => SysState st

class (Pretty trans, NFData trans) => Transition trans where
  runOneTest :: forall st
              . (SysState st, NFData st)
             => trans
             -> (trans -> Signal st)
             -> Signal TestResult

data TestResult = forall st trans
                . (Transition trans, Pretty st, SysState st, Pretty trans, NFData st, NFData trans)
               => TestResult { initConfig :: trans
                             , endSt      :: st
                             }
instance NFData TestResult where
  rnf a = seq a ()
instance Pretty TestResult where
  pPrint TestResult {..} = text "TestResult:"
                       $+$ text "initConfig ="
                       <+> (pPrint initConfig
                            $+$ text "endSt =" <+> pPrint endSt)
instance Show TestResult where
  show = show.pPrint
-----This will be used if no changes are needed -------------------------------
runOneTest' :: forall st trans
             . (SysState st, Pretty st, Transition trans, NFData st, NFData trans)
            => trans
            -> (trans -> Signal st)
            -> Signal TestResult
runOneTest' config topEntity' = TestResult config <$> result
  where
    result = topEntity' config

--------------------------------------------------------------------------------
--Testing Functions
--------------------------------------------------------------------------------
getTestResult :: forall st trans
              . (SysState st, Transition trans, NFData st, NFData trans)
              => Bool
              -> Int
              -> trans
              -> (trans -> Signal st)
              -> [TestResult]
getTestResult getTail howManyResults config topEntity'= (conTail.sampleN howManyResults) testR
  where
    conTail x = if getTail then P.tail x else x
    test = runOneTest
    testR = test config topEntity'

runConfigList :: forall st trans
               . (SysState st, Transition trans, NFData st, NFData trans)
              => (trans -> Signal st)
              -> [trans]
              -> [[TestResult]]
runConfigList = runConfigList' True 2

runConfigList' :: forall st trans
                . (SysState st, Transition trans, NFData st, NFData trans)
               => Bool
               -> Int
               -> (trans -> Signal st)
               -> [trans]
               -> [[TestResult]]
runConfigList' getTail howMany topEntity' = P.map test
  where
    test config = getTestResult getTail howMany config topEntity'
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Stuff to define in each file that wants to use the testing tools
--------------------------------------------------------------------------------
{-
instance PortIn PIn
instance SysState St

data Config = Config { input'  :: PIn
                     , startSt' :: St
                     }
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

--Probably no copy pasta here
instance Pretty PIn where
instance Pretty St where
configurationList :: [Config]

-}












--
