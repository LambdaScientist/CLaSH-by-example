{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures   #-}
{-# LANGUAGE MagicHash        #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TemplateHaskell  #-}

module ClocksAndRegisters.SimpleDFlop where

import qualified Prelude as P
import CLaSH.Prelude
import Control.Lens hiding ((:>))

--inputs
data PIn = PIn { _in_1 :: Bit
               , _clk  :: Bit
               } deriving (Eq)
data St = St { _out_1 :: Bit
             } deriving (Eq)
makeLenses ''St

onTrue :: St -> PIn -> Bool -> St
onTrue st PIn{..} condition = if condition then st{ _out_1 = _in_1 } else st

topEntity :: Signal PIn -> Signal St
topEntity = topEntity' startSt
  where
    startSt = St 0

topEntity' :: St -> Signal PIn -> Signal St
topEntity' st pin = result
  where
    result = register st (onTrue <$> result <*> pin <*> rising )
    rising = isRising 0 clk
    clk = _clk <$> pin

---TESTING
instance Show PIn where
  show PIn {..} =
    "PIn\n\t _in_1 = " P.++ show _in_1
    P.++ "\n\t _clk = " P.++ show _clk

instance Show St where
 show St {..} =
        "St\n\t _out_1 = " P.++ show _out_1
