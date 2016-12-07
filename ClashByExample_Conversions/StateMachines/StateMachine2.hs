{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures   #-}
{-# LANGUAGE MagicHash        #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TemplateHaskell  #-}


module StateMachine2 where

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
--Outputs and state data
data StateLabel = Idle | Active | Finish | Abort deriving (Show, Eq)

data St = St { _state_reg :: StateLabel
             , _count     :: BitVector 8
             , _done      :: Bool
             } deriving (Show, Eq)
makeLenses ''St



reset :: St
reset = St Idle 0 False


onRun :: St -> PIn -> Bool -> St
onRun st@St{..} PIn{..} risingEdge = flip execState st $ do
  if _reset then put reset
  else
    when risingEdge $ do
      case _state_reg of
        Idle   -> when _go $ do state_reg .= Active
                                count .= 0
                                done  .= False
        Active -> do count += 1
                     done .= False
                     if _kill then state_reg .= Abort
                     else when (_count == 64)  $ state_reg .= Finish
        Finish -> do count .= 0
                     done .= True
                     state_reg .= Idle
        Abort  -> do count .= 0
                     done .= False
                     unless _kill $ state_reg .= Idle
        otherwise  -> put reset
