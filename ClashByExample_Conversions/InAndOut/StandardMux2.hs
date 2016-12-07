{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures   #-}
{-# LANGUAGE MagicHash        #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TemplateHaskell  #-}


module StandardMux1 where

import qualified Prelude as P2
import CLaSH.Prelude
import Control.Lens hiding ((:>))
import Control.Monad.Trans.State

--inputs
data PIn = PIn { _in_1 :: BitVector 4
               , _in_2 :: BitVector 4
               , _in_3 :: Bit
               } deriving (Show, Eq)
--Outputs and state data
data St = St { _out_1 :: BitVector 4
             } deriving (Show, Eq)
makeLenses ''St

procSimple :: St -> PIn -> St
procSimple st@St{..} PIn{..} = flip execState st $
  out_1 .= if bit2Bool _in_3 then _in_2 else _in_1

bit2Bool :: Bit -> Bool
bit2Bool 1 = True
bit2Bool _ = False

topEntity :: St -> Signal PIn -> Signal St
topEntity st pin = reg
  where
    reg = register st (procSimple <$> reg <*> pin)

runTop :: Signal St
runTop = topEntity (St 0 ) (signal $ PIn 1 0 0)
