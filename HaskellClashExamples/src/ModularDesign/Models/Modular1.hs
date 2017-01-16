{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE NoImplicitPrelude  #-}

module ModularDesign.Models.Modular1 where

import CLaSH.Prelude

import Control.Lens hiding ((:>))
import Control.Monad.Trans.State
import Control.Monad

import SAFE.TestingTools
import SAFE.CommonClash

import Text.PrettyPrint.HughesPJClass

import qualified ModularDesign.Models.StateMachine as SM

data Partial = PPIn { _go'    :: Bool
                    , _kill'  :: Bool
                    } deriving (Eq, Show)
instance PortIn Partial
instance Pretty Partial where
  pPrint PPIn {..} = text "PPIn:"
                $+$ text "_go' ="   <+> showT _go'
                $+$ text "_kill' =" <+> showT _kill'

data MPIn = MPIn { _clk'     :: Bit
                 , _reset'   :: Bool
                 , _kill_clr :: Bool
                 , _pin1     :: Partial
                 , _pin2     :: Partial
                 , _pin3     :: Partial
                 } deriving (Eq, Show)
instance PortIn MPIn
instance Pretty MPIn where
  pPrint MPIn {..} = text "MPIn:"
               $+$ text "_clk' ="   <+> showT _clk'
               $+$ text "_reset' =" <+> showT _reset'
               $+$ text "_kill_clr =" <+> showT _kill_clr
               $+$ text "_pin1 =" <+> pPrint _pin1
               $+$ text "_pin2 =" <+> pPrint _pin2
               $+$ text "_pin3 =" <+> pPrint _pin3

--Outputs and state data

partial2full :: Partial -> Bit -> Bool -> SM.PIn
partial2full (PPIn go kill) clk reset = SM.PIn clk reset go kill

data StateLabel = Idle | Active | Finish | Abort deriving (Show, Eq)

data St = St { _state_reg :: SM.StateLabel
             , _count     :: BitVector 8
             , _done1      :: Bool
             , _done2      :: Bool
             , _done3      :: Bool
             , _kill_ltchd :: Bool
             } deriving (Eq, Show)
makeLenses ''St
instance SysState St
instance Pretty St where
  pPrint St {..} = text "St"
               $+$ text "_state_reg ="  <+>  showT _state_reg
               $+$ text "_count ="      <+>  showT _count
               $+$ text "_done1 ="      <+>  showT _done1
               $+$ text "_done2 ="      <+>  showT _done2
               $+$ text "_done3 ="      <+>  showT _done3
               $+$ text "_kill_ltchd =" <+>  showT _kill_ltchd


resetSt :: St -> St
resetSt (St x y z1 z2 z3  _) = St x y z1 z2 z3 False

partial :: Partial
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

setStDone:: St -> Bool -> Bool -> Bool -> St
setStDone st d1 d2 d3 = st {_done1 = d1, _done2 = d2, _done3 = d3}

topEntity :: Signal MPIn -> Signal St
topEntity = topEntity' st
  where
    st = St SM.Idle 0 False False False False

topEntity' :: St ->  Signal MPIn -> Signal St--Signal st
topEntity' st mp = setStDone <$> pin <*> (SM._done <$> s1) <*> (SM._done <$> s2) <*> (SM._done <$> s3)
  where
    startSt = SM.St SM.Idle 0 False
    pin = register (St SM.Idle 0 False False False False)
                   (runRegProc <$> pin <*> mp <*> signal False)
    sbool = signal False
    pin1 = _pin1 <$> mp
    pin2 = _pin2 <$> mp
    pin3 = _pin3 <$> mp
    clk = _clk' <$> mp
    reset = _reset' <$> mp
    s1 = register startSt  (SM.onRun <$> s1 <*> (partial2full <$>  pin1 <*>  clk <*> reset) <*> sbool)
    s2 = register startSt  (SM.onRun <$> s2 <*> (partial2full <$>  pin2 <*>  clk <*> reset) <*> sbool)
    s3 = register startSt  (SM.onRun <$> s3 <*> (partial2full <$>  pin3 <*>  clk <*> reset) <*> sbool)
