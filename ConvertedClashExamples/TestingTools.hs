{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
-- {-# LANGUAGE KindSignatures   #-}
{-# LANGUAGE MagicHash        #-}
{-# LANGUAGE RecordWildCards  #-}
-- {-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE RankNTypes  #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}

module TestingTools where

import qualified Prelude as P
import CLaSH.Prelude

--------------------------------------------------------------------------------
-- Abstract Area
--------------------------------------------------------------------------------
class (Eq pin, Show pin) => PortIn pin

class (Eq st, Show st) => SysState st

class (Show trans) => Transition trans where
  runOneTest :: forall st . (SysState st)
             => trans
             -> (trans -> Signal st)
             -> Signal TestResult

data TestResult = forall st trans . (Transition trans, Show st, SysState st) =>
                           TestResult { initConfig :: trans
                                      , endSt      :: st
                                      }
instance Show TestResult where
  show TestResult {..} =
         "TestResult:\n initConfig = " P.++ show initConfig
    P.++ "\n Result = " P.++ show endSt
    P.++ "\n\n"


-----This will be used if no changes are needed -------------------------------
runOneTest' :: forall st trans
             . (SysState st, Show st, Transition trans)
            => trans
            -> (trans -> Signal st)
            -> Signal TestResult
runOneTest' config topEntity' = TestResult config <$> result
  where
    result = topEntity' config

--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
--Testing Functions
--------------------------------------------------------------------------------
getTestResult :: forall st trans
              . (SysState st, Transition trans)
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
               . (SysState st, Transition trans)
              => (trans -> Signal st)
              -> [trans]
              -> [[TestResult]]
runConfigList = runConfigList' True 2

runConfigList' :: forall st trans
                . (SysState st, Transition trans)
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
-- Stuff to define in each file
--------------------------------------------------------------------------------
{-
instance PortIn PIn
instance SysState St

data Config = Config { input'  :: PIn
                     , startSt' :: St
                     }
instance Show Config where
  show Config{..} = "Config:\n input = " P.++ show input'
               P.++ "\n startSt = " P.++ show startSt'
instance  Transition Config where
  runOneTest = runOneTest'

setupTest :: Config -> Signal St
setupTest (Config pin st) = topEntity' st sPin
  where
    sPin = signal pin

setupAndRun :: [[TestResult]]
setupAndRun = runConfigList setupTest configurationList

--Probably no copy pasta here
instance Show PIn where
instance Show St where
configurationList :: [Config]

-}












--
