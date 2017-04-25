{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MagicHash        #-}

-- from http://hackage.haskell.org/package/clash-prelude-0.10.14/docs/CLaSH-Signal-Explicit.html#v:unsafeSynchronizer
module MultipleClocks where

import qualified Prelude as P
import CLaSH.Prelude
import CLaSH.Signal.Explicit


type Clk7 = Clk "clk7" 7 --This just makes a clock with period 7
clk7 :: SClock Clk7
clk7 = sclock

type Clk2 = Clk "clk2" 2  --This just makes a clock with period 2 which is 7/2 times faster then 7
clk2 :: SClock Clk2
clk2 = sclock

----------------------------------------------------------------------------
--THIS IS HERE ONLY TO SHOW TYPE SIGNATURES OF:
--unsafeSynchronizer
-- unsafeSynchronizer speeds up or slows down a signal to a new rate defined by the second argument
unsafeSynchronizer' :: SClock clk1 -- Clock of the incoming signal
                    -> SClock clk2 -- Clock of the outgoing signal
                    -> Signal' clk1 a  -- Signal coming in at clock 1 rate
                    -> Signal' clk2 a  -- Signal going out at clock 2 rate
unsafeSynchronizer' = undefined

-- Clock
-- data Clock = Clk Symbol Nat
-- A clock with a name (Symbol) and period (Nat)
----------------------------------------------------------------------------
oversampling :: Signal' Clk7 Integer -> Signal' Clk2 Integer
oversampling = register' clk2 99 . unsafeSynchronizer clk7 clk2
             . register' clk7 50

overSampleExample :: [Integer]
overSampleExample = sampleN 37 $ oversampling $ fromList [1..10]
-- 99,50,1,1,1,2,2,2,2,3,3,3,4,4,4,4,5,5,5,6,6,6,6,7,7,7,8,8,8,8,9,9,9,10,10,10,10

almostId :: Signal' Clk7 Integer -> Signal' Clk7 Integer
almostId = register' clk7 70 . unsafeSynchronizer clk2 clk7
         . register' clk2 99 . unsafeSynchronizer clk7 clk2
         . register' clk7 50

downSampleExample :: [Integer]
downSampleExample = sampleN 12 $ almostId $ fromList [1..10]
-- 70,99,1,2,3,4,5,6,7,8,9,10



dualFlipFlop :: SClock clkA -> SClock clkB
             -> Signal' clkA Bit -> Signal' clkB Bit
dualFlipFlop clkA clkB = register' clkB low . register' clkB low
                       . unsafeSynchronizer clkA clkB

dualFlipFlopExample :: [Bit]
dualFlipFlopExample = sampleN 42 runDualFlipFlop

runDualFlipFlop:: Signal' Clk7 Bit
runDualFlipFlop = dualFlipFlop clk2 clk7 oscillate
  where
    oscillate = register' clk2 0 ( (+) <$> oscillate <*> 1)
