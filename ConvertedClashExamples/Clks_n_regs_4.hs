{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures   #-}
{-# LANGUAGE MagicHash        #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE RankNTypes  #-}


module Clks_n_regs_4 where

import qualified Prelude as P
import CLaSH.Prelude
import Control.Lens hiding ((:>))
import Control.Monad.Trans.State
import Control.Monad

import TestProc


--inputs
data PIn = PIn { _clk   :: Bit
               , _reset :: Bool
               , _start :: Bool
               , _stop  :: Bool
               } deriving (Eq)
instance PortIn PIn
instance Show PIn where
  show PIn {..} =
    "PIn\n\t _clk = " P.++ show _clk
    P.++ "\n\t _reset = " P.++ show _reset
    P.++ "\n\t _start = " P.++ show _start
    P.++ "\n\t _stop = " P.++ show _stop


--Outputs and state data
data St = St { _cnt_en   :: Bool
             , _count_us :: BitVector 4
             , _stop_d1  :: Bool
             , _stop_d2  :: Bool
             , _count    :: BitVector 4
             } deriving (Eq)
makeLenses ''St
instance SysState St
instance Show St where
 show St {..} =
        "St\n\t _cnt_en = " P.++ show _cnt_en
   P.++ "\n\t _count_us = " P.++ show _count_us
   P.++ "\n\t _stop_d1 = " P.++ show _stop_d1
   P.++ "\n\t _stop_d2 = " P.++ show _stop_d2
   P.++ "\n\t _count = " P.++ show _count


resetSTKeepCount :: BitVector 4 -> St
resetSTKeepCount = St False 0 False False


onTrue :: St -> PIn -> Bool -> St
onTrue st@St{..} PIn{..} risingEdge = flip execState st $
  if _reset then put $ resetSTKeepCount _count
  else
    when risingEdge $ do
      --SR Flop
      if _start then  cnt_en .= True
      else when _stop $ cnt_en .= False
      --Counter
      when _cnt_en $ if _count_us == (13::BitVector 4) then count_us .= 0
                    else count_us += 1
      stop_d1 .= _stop
      stop_d2 .= _stop_d1

bnot :: Bit -> Bit
bnot 1 = 0
bnot _ = 1

-- topEntity :: Signal PIn -> Signal St
-- topEntity = topEntity' st
--   where
--     st = St False 0 False False 0

topEntity' :: SysStateShow st -> Signal (PortInShow pin) -> Signal (SysStateShow st)--Signal st
topEntity' st pin = result
  where
    result = register st result --(onTrue <$> result <*> pin <*> rising )
    -- rising = isRising 0 clk
    -- clk = _clk <$> pin
-- topEntity' :: St -> Signal PIn -> Signal St
-- topEntity' st pin = result
--   where
--     result = register st (onTrue <$> result <*> pin <*> rising )
--     rising = isRising 0 clk
--     clk = _clk <$> pin


---TESTING

data Config pin st = Config { input'  :: PortInShow pin
                            , startSt' :: SysStateShow st
                            }

instance Show (Config pin st) where
 show (Config pin st) =
        "Config:\n input = " P.++ show pin
   P.++ "\n startSt = " P.++ show st

-- instance Configuration Config where
--   input = input'
--   startSt = startSt'

instance (PortIn pin, SysState st) => Configuration (Config pin st) where
  input (Config pin _) = input'
  startSt (Config _ st) = st

instance Transition (Config pin st) where
  runOneTest = runOneTest'


configList :: [Config pin st]
configList = [configOne, configTwo, configThree, configFour]
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
