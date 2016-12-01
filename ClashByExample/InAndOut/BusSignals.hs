{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures   #-}
{-# LANGUAGE MagicHash        #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TemplateHaskell  #-}


module BusSignals where

import qualified Prelude as P
import CLaSH.Prelude
import Control.Lens hiding ((:>))
import Control.Monad.Trans.State
import CLaSH.Sized.Internal.BitVector

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
procSimple st@St{..} PIn{..} = flip execState st $ do
  out_1 .= keepValue (bnot _in_3) _in_1 .|. keepValue _in_3 _in_2
  where
    keepValue keepDecider =  v2bv.map (.&. keepDecider).bv2v


bnot :: Bit -> Bit
bnot 1 = 0
bnot _ = 1


bit2Bool :: Bit -> Bool
bit2Bool 1 = True
bit2Bool _ = False

topEntity :: St -> Signal PIn -> Signal St
topEntity st pin = reg
  where
    reg = register st (procSimple <$> reg <*> pin)

runTop :: Signal St
runTop = topEntity (St 0 ) (signal $ PIn 1 0 1)
