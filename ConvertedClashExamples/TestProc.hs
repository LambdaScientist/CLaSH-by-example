{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
-- {-# LANGUAGE KindSignatures   #-}
{-# LANGUAGE MagicHash        #-}
{-# LANGUAGE RecordWildCards  #-}
-- {-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE RankNTypes  #-}
{-# LANGUAGE ExistentialQuantification  #-}
-- {-# LANGUAGE StandaloneDeriving  #-}


module TestProc where

import qualified Prelude as P
import CLaSH.Prelude
-- import Control.Lens hiding ((:>))
-- import Control.Monad.Trans.State
-- import Control.Monad

--------------------------------------------------------------------------------
-- Abstract Area
--------------------------------------------------------------------------------
data TestResult = forall t s . (Show t, Transition t, Show s, SysState s) =>
                  TestResult { initConfig :: t
                             , endSt      :: s
                             }
instance Show TestResult where
  show TestResult {..} =
         "TestResult:\n initConfig = " P.++ show initConfig
    P.++ "\n Result = " P.++ show endSt
    P.++ "\n\n"


data Config = forall p s . (PortIn p, SysState s, Show p, Show s) =>
              Config { input   :: p
                     , startSt :: s
                     }
instance Show Config where
  show (Config p s) = "Config:\n input = " P.++ show p
                 P.++ "\n startS = " P.++ show s

class (Eq s, Show s) => SysState s

class Show t => Transition t where
  runOneTest :: forall st pin . (SysState st, PortIn pin)
             => t
             -> (st -> Signal pin -> Signal st)
             -> Signal TestResult

class Result r

class (Eq p, Show p) => PortIn p

instance Transition Config where
  runOneTest = runOneTest'

runOneTest' :: forall st pin . (SysState st, PortIn pin)
            => (st, pin)
            -> (st -> Signal pin -> Signal st)
            -> Signal TestResult
runOneTest' (startSt', input') topEntity' = TestResult (Config startSt' input') <$> result
  where
    result = topEntity' startingState inputSignal
    startingState = startSt'
    inputSignal   = signal input'
-- runOneTest' :: forall st pin . (SysState st, PortIn pin)
--             => Config
--             -> (st -> Signal pin -> Signal st)
--             -> Signal TestResult
-- runOneTest' config@Config{..} topEntity' = TestResult config <$> result
--   where
--     result = topEntity' startingState inputSignal
--     startingState = startSt
--     inputSignal   = signal input

hack (Config x y) = (x,y)
--------------------------------------------------------------------------------
--Flexible Area
--------------------------------------------------------------------------------
getTestResult :: forall st pin . (SysState st, PortIn pin)
              => Bool -> Int -> Config -> (st -> Signal pin -> Signal st) -> [TestResult]
getTestResult getTail howManyResults config topEntity'= conTail $ sampleN howManyResults test
  where
    conTail x = if getTail then P.tail x else x
    test      = runOneTest config topEntity'

runConfigList :: forall st pin . (SysState st, PortIn pin)
              => (st -> Signal pin -> Signal st) -> [Config] -> [[TestResult]]
runConfigList = runConfigList' True 2

runConfigList' :: forall st pin . (SysState st, PortIn pin)
               => Bool -> Int -> (st -> Signal pin -> Signal st) -> [Config] -> [[TestResult]]
runConfigList' getTail howMany topEntity' = P.map test
  where
    test config = getTestResult getTail howMany config topEntity'


--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Stuff to define in each file
--------------------------------------------------------------------------------
-- start :: St
-- start  = St False 0 False False 0
--
-- defaultTest :: [[TestResult]]
-- defaultTest = runConfigList configList
--
-- inputOne :: PIn
-- inputOne  = PIn 0 False False False
--
-- configOne :: Config
-- configOne = Config inputOne start
--
-- runOneTest' :: (Transition t, Show t) => t -> Signal TestResult
-- runOneTest' config = signal $ TestResult config st
--
-- data PIn = PIn { _clk   :: Bit
--                , _reset :: Bool
--                , _start :: Bool
--                , _stop  :: Bool
--                } deriving (Eq, Show)
-- instance PortIn PIn
--
-- data St = St { _cnt_en   :: Bool
--              , _count_us :: BitVector 4
--              , _stop_d1  :: Bool
--              , _stop_d2  :: Bool
--              , _count    :: BitVector 4
--              } deriving (Eq, Show)
-- instance SysState St
--
-- st :: St
-- st = St False 0 False False 0
--
-- instance Result TestResult
--

















--
