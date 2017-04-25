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

module UART (uart) where

import CLaSH.Prelude
import Control.Lens hiding ((:>))
import Control.Monad
import Control.Monad.Trans.State
import UARTTools

--Debug
import qualified Prelude as P


-- UART RX Logic
data RxReg = RxReg { _rx_reg        :: BitVector 8 --Where we store the data while reading
                   , _rx_data       :: BitVector 8 --Where we store the data after we read
                   , _rx_sample_cnt :: Unsigned 4  --Used to make sure your read only 1 bit inside of a 16 bit string
                   , _rx_cnt        :: Unsigned 4  --Index of where we are storeing the current bit we are reading
                   , _rx_frame_err  :: Bool        --True when Start/Stop Bit not found
                   , _rx_over_run   :: Bool        --Cannot process the character that just came in before the next one arrives.
                   , _rx_empty      :: Bool        --If we have nothing in the register
                   , _rx_d1         :: Bit         --The input from last round
                   , _rx_d2         :: Bit         --The input 3 rounds ago
                   , _rx_busy       :: Bool }      --Are we busy reading
makeLenses ''RxReg
instance Show RxReg where
  show RxReg {..} =
         "\nRxReg:\n _rx_reg = " P.++ show _rx_reg
    P.++ "\n _rx_data = " P.++ show _rx_data
    P.++ "\n _rx_sample_cnt = " P.++ show _rx_sample_cnt
    P.++ "\n _rx_cnt = " P.++ show _rx_cnt
    P.++ "\n _rx_frame_err = " P.++ show _rx_frame_err
    P.++ "\n _rx_over_run = " P.++ show _rx_over_run
    P.++ "\n _rx_empty = " P.++ show _rx_empty
    P.++ "\n _rx_d1 = " P.++ show _rx_d1
    P.++ "\n _rx_d2 = " P.++ show _rx_d2
    P.++ "\n _rx_busy = " P.++ show _rx_busy

uartRX :: RxReg -> Bit -> Bool -> Bool -> RxReg
uartRX r@RxReg {..} rx_in uld_rx_data rx_enable = flip execState r $ do
  -- Synchronise the async ssignal
  rx_d1 .= rx_in  -- Save our input for later use
  rx_d2 .= _rx_d1 -- Rx_d2 now holds our input from 3 iterations ago
  -- Uload the rx data
  when uld_rx_data $ do -- When uld_rx_data move the Value we were reading to the Output
    rx_data  .= _rx_reg
    rx_empty .= True
  -- Receive data only when rx is enabled
  if rx_enable then do
    -- Check if just received start of frame
    when (not _rx_busy && _rx_d2 == 0) $ do
      rx_busy       .= True
      -- Start of frame detected, Proceed with rest of data
      rx_sample_cnt .= 1
      rx_cnt        .= 0
    when _rx_busy $ do --When we are reading input
      rx_sample_cnt += 1
      -- Logic to sample at middle of data
      when (_rx_sample_cnt == 7) $
        if _rx_d1 == 1 && _rx_cnt == 0 then
          rx_busy .= False
        else do
          rx_cnt += 1 --Next rounds new index
          -- start storing the rx data
          when (_rx_cnt > 0 && _rx_cnt < 9) $
            rx_reg %= replaceBit (_rx_cnt - 1) _rx_d2 --Store what we read 3 rounds ago
          when (_rx_cnt == 9) $ do
            rx_busy .= False
            -- Check if End of frame received correctly
            if _rx_d2 == 0 then
              rx_frame_err .= True
            else do
              rx_empty     .= False
              rx_frame_err .= False
              -- Check if last rx data was not unloaded
              rx_over_run  .= not _rx_empty
  else rx_busy .= False

-- UART TX Logic
data TxReg
  = TxReg
  { _tx_reg      :: BitVector 8 --Where we store the data while writing
  , _tx_empty    :: Bool        --If we have nothing in the register
  , _tx_over_run :: Bool        --
  , _tx_out      :: Bit         --The bit we are sending out to be read
  , _tx_cnt      :: Unsigned 4  --When item are we writing
  , _repeatBit   :: Int }        --Used to mimic a slow clock
makeLenses ''TxReg
instance Show TxReg where
  show TxReg {..} =
         "\nTxReg:\n _tx_reg = " P.++ show _tx_reg
    P.++ "\n _tx_empty = " P.++ show _tx_empty
    P.++ "\n _tx_over_run = " P.++ show _tx_over_run
    P.++ "\n _tx_out = " P.++ show _tx_out
    P.++ "\n _tx_cnt = " P.++ show _tx_cnt
    P.++ "\n _repeatBit = " P.++ show _repeatBit

uartTX :: TxReg -> Bool -> BitVector 8 -> Bool -> TxReg
uartTX t@TxReg {..} ld_tx_data tx_data tx_enable = flip execState t $ do
  when (_repeatBit >= 15) $ do
    repeatBit.=0   --Reset our count
    when ld_tx_data $
      if not _tx_empty then
        tx_over_run .= False
      else do
        tx_reg   .= tx_data   --Move the data we want to transmit to the
        tx_empty .= False
    when (tx_enable && not _tx_empty) $ do
      tx_cnt += 1
      when (_tx_cnt == 0) $
        tx_out .= 0
      when (_tx_cnt > 0 && _tx_cnt < 9) $
        tx_out .= _tx_reg ! (_tx_cnt - 1) --Transmit the Bitvector one bit at a time.
      when (_tx_cnt == 9) $ do
        tx_out   .= 1
        tx_cnt   .= 0
        tx_empty .= True
    unless tx_enable $
      tx_cnt .= 0
  unless (_repeatBit >= 15) $ repeatBit+=1

-- Combine RX and TX logic
getUartData :: (Signal RxReg, Signal TxReg) -> (Signal Bit, Signal Bool, Signal (BitVector 8), Signal Bool)
getUartData (rxReg,txReg) =  ( _tx_out   <$> txReg
                             , _tx_empty <$> txReg
                             , _rx_data  <$> rxReg
                             , _rx_empty <$> rxReg )
uart :: Signal Bool
     -> Signal (BitVector 8)
     -> Signal Bool
     -> Signal Bit
     -> Signal Bool
     -> Signal Bool
     -> (Signal RxReg, Signal TxReg)
uart ld_tx_data tx_data tx_enable rx_in uld_rx_data rx_enable = (rxReg, txReg)
  where
    rxReg     = register rxRegInit (uartRX <$> rxReg <*> rx_in <*> uld_rx_data <*> rx_enable)
    rxRegInit = RxReg { _rx_reg        = 0
                      , _rx_data       = 0
                      , _rx_sample_cnt = 0
                      , _rx_cnt        = 0
                      , _rx_frame_err  = False
                      , _rx_over_run   = False
                      , _rx_empty      = True
                      , _rx_d1         = 1
                      , _rx_d2         = 1
                      , _rx_busy       = False }
    txReg     = register txRegInit $ uartTX <$> txReg <*> ld_tx_data <*> tx_data <*> tx_enable
    txRegInit = TxReg { _tx_reg      = 0
                      , _tx_empty    = True
                      , _tx_over_run = False
                      , _tx_out      = 1
                      , _tx_cnt      = 0
                      , _repeatBit   = 16 }
{-# ANN topEntity
  (defTop
    { t_name     = "UART"
    , t_outputs  = ["RxReg1", "TxReg1", "RxReg2", "TxReg2"]
    }) #-}
topEntity :: (Signal RxReg, Signal TxReg, Signal RxReg, Signal TxReg)
topEntity = twoUart

---------------------------------------------------------------------------------------------
----MY TEST CODE-----------------------------------------------------------------------------
---------------------------------------------------------------------------------------------


--------------------------------
-- Make Two UARTs
--------------------------------
twoUart :: (Signal RxReg, Signal TxReg, Signal RxReg, Signal TxReg)
twoUart = (rx1,tx1,rx2,tx2)--   ld_tx_data tx_data tx_enable rx_in uld_rx_data rx_enable
  where
  uartOneMessage = signal $ toBit8 'U'
  uartTwoMessage = signal $ toBit8 'e'
  --UART 1
  uart1 = uart _ld_tx_data uartOneMessage _tx_enable txOut2 _uld_rx_data _rx_enable
  (rx1,tx1) = uart1
  (txOut1,txEm1,rxData1,rxEm1) = getUartData uart2
  --UART 2
  uart2 =  uart _ld_tx_data uartTwoMessage _tx_enable txOut1 _uld_rx_data _rx_enable
  (rx2,tx2) = uart2
  (txOut2,txEm2,rxData2,rxEm2) = getUartData uart2

--------------------------------
-- Make UART loop
--------------------------------
uartLoop :: Signal Bool
         -> Signal (BitVector 8)
         -> Signal Bool
         -> Signal Bool
         -> Signal Bool
         -> (Signal Bit, Signal Bool, Signal (BitVector 8), Signal Bool)
uartLoop ld_tx_data tx_data tx_enable uld_rx_data rx_enable =
    ( txOut
    , _tx_empty <$> txReg
    , _rx_data  <$> rxReg
    , _rx_empty <$> rxReg )
  where
    rx_in = txOut
    txOut = _tx_out <$> txReg
    rxReg = register rxRegInit (uartRX <$> rxReg <*> rx_in <*> uld_rx_data <*> rx_enable)
    txReg = register txRegInit (uartTX <$> txReg <*> ld_tx_data <*> tx_data <*> tx_enable)
    rxRegInit = RxReg { _rx_reg        = 0
                      , _rx_data       = 0
                      , _rx_sample_cnt = 0
                      , _rx_cnt        = 0
                      , _rx_frame_err  = False
                      , _rx_over_run   = False
                      , _rx_empty      = True
                      , _rx_d1         = 1
                      , _rx_d2         = 1
                      , _rx_busy       = False }
    txRegInit = TxReg { _tx_reg      = 0
                      , _tx_empty    = True
                      , _tx_over_run = False
                      , _tx_out      = 1
                      , _tx_cnt      = 0
                      , _repeatBit   = 16 }
    ---------------------------
    -- Use these when debugging
    ---------------------------
    --printHackRX rx = (\x -> trace ("Last State RX: " P.++ (show  x)) x ) <$>  rx
    --printHackInput rx  = (\x y -> trace ("\n\n _tx_out = "  P.++ ((show $ x))) y ) <$>  temp <*> rx
    --printHackTX tx  = (\x -> trace ("Last State TX: "  P.++ (show $ x)) x ) <$>  tx

--------------------------------
-- Test uartLoop
--------------------------------
data DebugOut = DebugOut { charIn  :: Char
                        , bvIn    :: BitVector 8
                        , charOut :: Char
                        , bvOut   :: BitVector 8
                        }

instance Show DebugOut where
  show DebugOut {..} =    "\n\n charIn  = " P.++ show charIn
                     P.++ "\n charOut = "   P.++ show charOut
                     P.++ "\n bvIn  = "     P.++ show bvIn
                     P.++ "\n bvOut = "     P.++ show bvOut

charDebug :: Char -> Signal DebugOut
charDebug charInput = liftA (\x ->  DebugOut charInput inputCharBV (toBitChar x) x) flipDataResult
  where
    result = uartLoop _ld_tx_data (signal inputCharBV) _tx_enable _uld_rx_data _rx_enable
    inputCharBV =  toBitBV charInput
    flipDataResult =  result^._3

getSample :: Char -> [DebugOut]
getSample charInput = P.drop 159 $ sampleN 160 $ charDebug charInput

stringDebug :: [[DebugOut]] --Call this for testing
stringDebug = P.map getSample "aAbBgGuU4917!@#$*&^nM"

--Call These for  viewing the output of the UART loop
testUart :: (Signal Bit, Signal Bool, Signal (BitVector 8), Signal Bool)
testUart =  uartLoop _ld_tx_data _tx_data _tx_enable _uld_rx_data _rx_enable

checkTxOut   :: Signal (BitVector 1)
checkTxOut   = liftA reverseBV (testUart ^. _1)

checkTxEmpty :: Signal Bool
checkTxEmpty = testUart ^. _2

checkData    :: Signal (BitVector 8)
checkData    =  liftA reverseBV (testUart ^. _3)

checkEmpty :: Signal Bool
checkEmpty   = testUart ^. _4

--------------------------------
-- Test RX Packet Receiving
--------------------------------
message :: Vec 10 Bit
message = low  :>
          high :> high :> low  :> low  :>
          low :> high  :> high  :> high :>
          high :> Nil
input :: Signal Bit
input  = (stimuliGenerator.repeat16x) message
uld :: Signal Bool
uld = stimuliGenerator (False :> Nil)
enable :: Signal Bool
enable  = stimuliGenerator (True:> Nil)
uartTestRx :: Signal Bit -> Signal Bool -> Signal Bool -> Signal RxReg
uartTestRx rx_in rx_unload rx_enable = rxReg
  where
   rxReg     = register rxRegInit (uartRX <$> rxReg <*> rx_in <*> rx_unload <*> rx_enable)
   rxRegInit = RxReg { _rx_reg        = 0
                     , _rx_data       = 0
                     , _rx_sample_cnt = 0
                     , _rx_cnt        = 0
                     , _rx_frame_err  = False
                     , _rx_over_run   = False
                     , _rx_empty      = True
                     , _rx_d1         = 1
                     , _rx_d2         = 1
                     , _rx_busy       = False }
   ---------------------------
   -- Use these when debugging
   ---------------------------
   --printHackRX rx = (\x -> trace ("Last State RX: " P.++ (show  x)) x ) <$>  rx
   --printHackInput rx  = (\x y -> trace ("\n\n _tx_out = "  P.++ ((show $ x))) y ) <$>  temp <*> rx
testRx :: Signal RxReg
testRx = uartTestRx input uld enable
outputTestRx :: [RxReg]
outputTestRx = P.drop 159 $ sampleN 160 testRx
