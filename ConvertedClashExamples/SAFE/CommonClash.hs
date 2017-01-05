{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MagicHash        #-}
{-# LANGUAGE RankNTypes  #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}

module SAFE.CommonClash (bnot) where

import qualified Prelude as P
import CLaSH.Prelude

bnot :: Bit -> Bit
bnot 1 = 0
bnot _ = 1

bit2Bool :: Bit -> Bool
bit2Bool 1 = True
bit2Bool _ = False
