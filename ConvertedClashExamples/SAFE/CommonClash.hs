{-# LANGUAGE NoImplicitPrelude #-}

module SAFE.CommonClash (bnot) where

import Text.PrettyPrint.HughesPJClass

import qualified Prelude as P
import CLaSH.Prelude

bnot :: Bit -> Bit
bnot 1 = 0
bnot _ = 1

bit2Bool :: Bit -> Bool
bit2Bool 1 = True
bit2Bool _ = False

--------------------------------------------------------------------------------
-- Functions to help with understanding test results
--------------------------------------------------------------------------------
showT :: (Show s) => s -> Doc
showT = text.show
