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


module TestProc where

import qualified Prelude as P
import CLaSH.Prelude
-- import Control.Lens hiding ((:>))
-- import Control.Monad.Trans.State
-- import Control.Monad

--------------------------------------------------------------------------------
-- Abstract Area
--------------------------------------------------------------------------------
data TestResult trans st = (Show trans, Transition trans, Configuration trans, Show st, SysState st) =>
                           TestResult { initConfig :: trans
                                      , endSt      :: SysStateShow st
                                      }
-- data TestResult trans st = (Show trans, Transition trans, Configuration trans, Show st, SysState st) =>
--                            TestResult { initConfig :: trans
--                                       , endSt      :: st
--                                       }
instance Show (TestResult trans st) where
  show TestResult {..} =
         "TestResult:\n initConfig = " P.++ show initConfig
    P.++ "\n Result = " P.++ show endSt
    P.++ "\n\n"

data PortInShow pin = (PortIn pin,   Show pin) => PortInShow pin
instance Show (PortInShow pin) where
  show (PortInShow pin) = "pin: = " P.++ show pin
  
data SysStateShow st = (SysState st, Show st) => SysStateShow st
instance Show (SysStateShow st) where
  show (SysStateShow st) = "St: = " P.++ show st

class Configuration c where
  input   :: c -> PortInShow pin
  startSt :: c -> SysStateShow st



-- class Configuration c where
--   input :: (PortIn pin, Show pin) => c -> pin
--   startSt :: (SysState st, Show st) => c -> st

-- data Config = forall st pin trans
--             . (SysState st, PortIn pin)
--             => Config { input'   :: pin
--                       , startSt' :: st
--                       }
--
--
-- instance Configuration Config where
--   input Config{input' = foo } = foo
--   startSt = startSt'

-- class Result result

class (Eq pin, Show pin) => PortIn pin

class (Eq st, Show st) => SysState st

class (Show trans, Configuration trans) => Transition trans where
  runOneTest :: forall st pin . (SysState st, PortIn pin)
             => trans
             -> (SysStateShow st -> Signal (PortInShow pin) -> Signal (SysStateShow st))
             -> Signal (TestResult trans st)
  -- runOneTest :: forall st pin . (SysState st, PortIn pin)
  --            => trans
  --            -> (SysStateShow -> Signal PortInShow -> Signal SysStateShow)
  --            -> Signal (TestResult trans st)

-----This will be used if no changes are needed -------------------------------
-- runOneTest' :: forall st pin trans
--              . (SysState st, PortIn pin, Transition trans, Configuration trans, Show st)
--             => trans
--             -> (SysStateShow -> Signal PortInShow -> Signal SysStateShow)
--             -> Signal (TestResult trans st)
-- runOneTest' config topEntity' = TestResult config <$> result
--   where
--     result = topEntity' startingState inputSignal
--     startingState = startSt config
--     inputSignal   = signal $ input config
runOneTest' :: forall st pin trans
             . (SysState st, PortIn pin, Transition trans, Configuration trans, Show st)
            => trans
            -> (SysStateShow st -> Signal (PortInShow pin) -> Signal (SysStateShow st))
            -> Signal (TestResult trans st)
runOneTest' config topEntity' = TestResult config <$> result
  where
    result = topEntity' startingState inputSignal
    startingState = startSt config
    inputSignal   = signal $ input config
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
--Flexible Area
--------------------------------------------------------------------------------
getTestResult :: forall st pin trans
              . (SysState st, PortIn pin, Transition trans)
              => Bool
              -> Int
              -> trans
              -> (SysStateShow st -> Signal (PortInShow pin) -> Signal (SysStateShow st))
              -> [TestResult trans st]
getTestResult getTail howManyResults config topEntity'= (conTail.sampleN howManyResults) testR
  where
    conTail x = if getTail then P.tail x else x
    test :: trans -> (SysStateShow st -> Signal (PortInShow pin) -> Signal (SysStateShow st)) ->  Signal (TestResult trans st)
    test = runOneTest
    testR = test config topEntity'
-- --
runConfigList :: forall st pin trans
               . (SysState st, PortIn pin, Transition trans)
              => (SysStateShow st -> Signal (PortInShow pin) -> Signal (SysStateShow st))
              -> [trans]
              -> [[TestResult trans st]]
runConfigList = runConfigList' True 2
--
runConfigList' :: forall st pin trans
                . (SysState st, PortIn pin, Transition trans)
               => Bool
               -> Int
               -> (SysStateShow st -> Signal (PortInShow pin) -> Signal (SysStateShow st))
               -> [trans]
               -> [[TestResult trans st]]
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
