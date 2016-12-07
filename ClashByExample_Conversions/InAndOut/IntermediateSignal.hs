{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures   #-}
{-# LANGUAGE MagicHash        #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TemplateHaskell  #-}


module IntermediateSignal where

import qualified Prelude as P
import CLaSH.Prelude
import Control.Lens hiding ((:>))
import Control.Monad.Trans.State

--inputs
data PIn = PIn { _in_1 :: Bit
               , _in_2 :: Bit
               , _in_3 :: Bit
               } deriving (Show, Eq)
--Outputs and state data
data St = St { _out_1 :: Bit
             , _out_2 :: Bit
             } deriving (Show, Eq)
makeLenses ''St

procSimple :: St -> PIn -> St
procSimple st@St{..} PIn{..} = flip execState st $ do
  out_1 .= ( intermediate_sig .&. _in_3)
  out_2 .= ( intermediate_sig .|. _in_3)
  where
    intermediate_sig = _in_1 .&. _in_2

topEntity :: St -> Signal PIn -> Signal St
topEntity st pin = reg
  where
    reg = register st (procSimple <$> reg <*> pin)

runTop :: Signal St
runTop = topEntity (St 0 0) (signal $ PIn 1 0 1)
