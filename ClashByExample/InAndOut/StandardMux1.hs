{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures   #-}
{-# LANGUAGE MagicHash        #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TemplateHaskell  #-}


module StandardMux1 where

import qualified Prelude as P
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
  out_1 .= multiPlex _in_3 _in_1 _in_2

multiPlex :: Bit -> BitVector 4 -> BitVector 4 -> BitVector 4
multiPlex 1 _ y = y
multiPlex 0 x _ = x
multiPlex _ _ _ = 0

topEntity :: St -> Signal PIn -> Signal St
topEntity st pin = reg
  where
    reg = register st (procSimple <$> reg <*> pin)

runTop :: Signal St
runTop = topEntity (St 0 ) (signal $ PIn 1 0 0)
