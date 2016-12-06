{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures   #-}
{-# LANGUAGE MagicHash        #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TemplateHaskell  #-}


module Modular1 where

import qualified Prelude as P
import CLaSH.Prelude
import Control.Lens hiding ((:>))
import Control.Monad.Trans.State
import Control.Monad
import CLaSH.Sized.Internal.BitVector

-- import CLaSH.Signal.Delayed.Explicit


--inputs
data PIn = PIn { _clk   :: Bit
               , _reset :: Bool
               , _go    :: Bool
               , _kill  :: Bool
               } deriving (Show, Eq)
data Partial = PPIn { _go'    :: Bool
                    , _kill'  :: Bool
                    } deriving (Show, Eq)
data MPIn = MPIn { _clk'     :: Bit
                 , _reset'   :: Bool
                 , _kill_clr :: Bool
                 , _pin1     :: Partial
                 , _pin2     :: Partial
                 , _pin3     :: Partial
                 } deriving (Show, Eq)
--Outputs and state data

partial2full (PPIn go kill) clk reset = PIn clk reset go kill

partial2fullLogic (PPIn go kill) clk reset comp = PIn clk reset (go || comp) kill

data StateLabel = Idle | Active | Finish | Abort deriving (Show, Eq)

data St = St { _state_reg :: StateLabel
             , _count     :: BitVector 8
             , _done      :: Bool
             , _kill_ltchd :: Bool
             } deriving (Show, Eq)
makeLenses ''St



resetSt :: St -> St
resetSt (St x y z _) = St x y z False

partial = PPIn False False

runRegProc :: St -> MPIn -> Bool -> St
runRegProc st@St{..} MPIn{..} rising = flip execState st $
  if _reset' then put $ resetSt st
  else when rising $
    if kill_1 || kill_2 || kill_3 then kill_ltchd .= True
      else when _kill_clr $ kill_ltchd .= False
  where
    kill_1 = _kill' _pin1
    kill_2 = _kill' _pin2
    kill_3 = _kill' _pin3


runSt :: St -> PIn -> Bool -> St
runSt st@St{..} PIn{..} risingEdge = flip execState st $ do
  if _reset then put $ resetSt st
  else
    case _state_reg of
      Idle   -> when _go $ state_reg .= Active
      Active -> if _kill then state_reg .= Abort else when (_count == 64)  $ state_reg .= Finish
      Finish -> state_reg .= Idle
      Abort  -> unless _kill $ state_reg .= Idle
      _  -> state_reg .= Idle
  when risingEdge $ do
    if _state_reg == Finish || _state_reg == Abort then count .= 0
                                                   else when (_state_reg == Active) $ count += 1
    if _state_reg == Finish then done .= True else done .= False



run3State mp@MPIn{..} = (_done <$> s3, _kill_ltchd)
  where
    pin = register (resetSt (St Idle 0 False False)) (runRegProc <$> pin <*> signal mp <*> signal False)
    sbool = signal False
    s1 = register (resetSt (St Idle 0 False False)) (runSt <$> s1 <*> (signal $ partial2full _pin1 _clk' _reset' ) <*> sbool)
    s1Go = _done <$> s1
    s2 = register (resetSt (St Idle 0 False False)) (runSt <$> s2 <*> (partial2fullLogic _pin2 _clk' _reset' <$> s1Go)    <*> sbool)
    s2Go = _done <$> s2
    s3 = register (resetSt (St Idle 0 False False)) (runSt <$> s3 <*> (partial2fullLogic _pin3 _clk' _reset' <$> s2Go) <*> sbool)
