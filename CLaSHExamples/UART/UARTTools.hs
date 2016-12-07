{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ExplicitForAll #-}

module UARTTools where

import CLaSH.Prelude
import CLaSH.Sized.Internal.BitVector

--Debug
import qualified Prelude as P
import System.Random


---------------------------------------------------------------------------------------------
----Utility----------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------
toBit8 :: (Enum c, Enum a) => a -> c
toBit8 = toEnum.fromEnum

toBitBV :: Enum a => a -> BitVector 8
toBitBV = toBit8

toBitChar :: Enum a => a -> Char
toBitChar = toBit8

reverseBV :: KnownNat n => BitVector n -> BitVector n
reverseBV = v2bv . reverse . bv2v

repeat16x :: Vec n a -> Vec (n * 16) a
repeat16x = concat . (map (replicate d16))

repeat8x :: KnownNat n => BitVector n -> Vec n (BitVector 8)
repeat8x x = map v2bv $ (map (replicate d8)) $ bv2v x

fuzzBV :: KnownNat n => Int -> BitVector n -> BitVector n
fuzzBV pick bv2Fuzz = P.foldl (\y x ->  replaceBit# y x (invBitI x)) bv2Fuzz indexList
  where
    g = mkStdGen 42 --Add a seed later
    randomNum = P.take pick . P.zip [1..] . randomRs (0,limit) $ g
    limit = size# bv2Fuzz
    indexList = P.map ( \x -> (snd x) `mod` limit) randomNum
    invBitI = (\x -> complement $ index# bv2Fuzz x)

---------------------------------------------------------------------------------------------
----Constants----------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------
_ld_tx_data  :: Signal Bool
_ld_tx_data  =  signal True

_tx_data     :: Signal (BitVector 8)
_tx_data     = signal $ toBit8 'a'

_tx_enable   :: Signal Bool
_tx_enable   = signal True

_rx_in       :: Signal Bit
_rx_in       = signal high

_uld_rx_data :: Signal Bool
_uld_rx_data =  signal True

_rx_enable   :: Signal Bool
_rx_enable   = signal True

oscillateFT :: Signal Bool
oscillateFT = register False (not1 oscillateFT)

oscillateTF :: Signal Bool
oscillateTF = register True (not1 oscillateTF)
