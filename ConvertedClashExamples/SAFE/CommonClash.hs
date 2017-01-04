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
