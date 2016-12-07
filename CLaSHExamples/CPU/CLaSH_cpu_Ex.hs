{-# LANGUAGE RecordWildCards #-}
module CPU where

import CLaSH.Prelude
import qualified Data.List as L


topEntity =  L.tail $ sampleN 33 $ system3 prog2


type InstrAddr = Unsigned 8
type MemAddr   = Unsigned 5
type Value     = Signed 8
data Instruction
  = Compute Operator Reg Reg Reg
  | Branch Reg Value
  | Jump Value
  | Load MemAddr Reg
  | Store Reg MemAddr
  | Nop
  deriving (Eq,Show)
data Reg
  = Zero
  | PC
  | RegA
  | RegB
  | RegC
  | RegD
  | RegE
  deriving (Eq,Show,Enum)
data Operator = Add | Sub | Incr | Imm | CmpGt
  deriving (Eq,Show)
data MachCode
  = MachCode
  { inputX  :: Reg
  , inputY  :: Reg
  , result  :: Reg
  , aluCode :: Operator
  , ldReg   :: Reg
  , rdAddr  :: MemAddr
  , wrAddr  :: MemAddr
  , wrEn    :: Bool
  , jmpM    :: Maybe Value
  }
nullCode = MachCode { inputX = Zero, inputY = Zero, result = Zero, aluCode = Imm
                    , ldReg = Zero, wrAddr = 0, rdAddr = 0, wrEn = False
                    , jmpM = Nothing
                    }

--------------------------------------------------------------------------------
cpu :: Vec 7 Value          -- ^ Register bank
    -> (Value,Instruction)  -- ^ (Memory output, Current instruction)
    -> ( Vec 7 Value
       , (MemAddr,MemAddr,Bool,Value,InstrAddr)
       )
cpu regbank (memOut,instr) = (regbank',(rdAddr,wrAddr,wrEn,aluOut,fromIntegral ipntr))
  where
    -- Current instruction pointer
    ipntr = regbank !! PC
    -- Decoder
    (MachCode {..}) = case instr of
      Compute op rx ry res -> nullCode {inputX=rx,inputY=ry,result=res,aluCode=op}
      Branch cr a          -> nullCode {inputX=cr,jmpM=Just a}
      Jump a               -> nullCode {aluCode=Incr,jmpM=Just a}
      Load a r             -> nullCode {ldReg=r,rdAddr=a}
      Store r a            -> nullCode {inputX=r,wrAddr=a,wrEn=True}
      Nop                  -> nullCode
    -- ALU
    regX   = regbank !! inputX
    regY   = regbank !! inputY
    aluOut = alu aluCode regX regY
    -- next instruction
    nextPC = case jmpM of
               Just a | aluOut /= 0 -> ipntr + a
               _                    -> ipntr + 1
    -- update registers
    regbank' = replace Zero   0
             $ replace PC     nextPC
             $ replace result aluOut
             $ replace ldReg  memOut
             $ regbank
alu Add   x y = x + y
alu Sub   x y = x - y
alu Incr  x _ = x + 1
alu Imm   x _ = x
alu CmpGt x y = if x > y then 1 else 0
dataMem :: Signal MemAddr -- ^ Read address
        -> Signal MemAddr -- ^ Write address
        -> Signal Bool    -- ^ Write enable
        -> Signal Value   -- ^ data in
        -> Signal Value   -- ^ data out
dataMem wr rd en din = mealy dataMemT (replicate d32 0) (bundle (wr,rd,en,din))
  where
    dataMemT mem (wr,rd,en,din) = (mem',dout)
      where
        dout = mem !! rd
        mem' | en        = replace wr din mem
             | otherwise = mem
system :: KnownNat n => Vec n Instruction -> Signal Value
system instrs = memOut
  where
    memOut = dataMem wrAddr rdAddr wrEn aluOut
    (rdAddr,wrAddr,wrEn,aluOut,ipntr) = mealyB cpu (replicate d7 0) (memOut,instr)
    instr  = asyncRom instrs <$> ipntr
--------------------------------------------------------------------------------
system2 :: KnownNat n => Vec n Instruction -> Signal Value
system2 instrs = memOut
  where
    memOut = asyncRam d32 wrAddr rdAddr wrEn aluOut
    (rdAddr,wrAddr,wrEn,aluOut,ipntr) = mealyB cpu (replicate d7 0) (memOut,instr)
    instr  = asyncRom instrs <$> ipntr
prog = -- 0 := 4
   Compute Incr Zero RegA RegA :>
   replicate d3 (Compute Incr RegA Zero RegA) ++
   Store RegA 0 :>
   -- 1 := 6
   Compute Incr Zero RegA RegA :>
   replicate d5 (Compute Incr RegA Zero RegA) ++
   Store RegA 1 :>
   -- A := 4
   Load 0 RegA :>
   -- B := 6
   Load 1 RegB :>
   -- start
   Compute CmpGt RegA RegB RegC :>
   Branch RegC 4 :>
   Compute CmpGt RegB RegA RegC :>
   Branch RegC 4 :>
   Jump 5 :>
   -- (a > b)
   Compute Sub RegA RegB RegA :>
   Jump (-6) :>
   -- (b > a)
   Compute Sub RegB RegA RegB :>
   Jump (-8) :>
   -- end
   Store RegA 2 :>
   Load 2 RegC :>
   Nil
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------

cpu2 :: (Vec 7 Value,Reg)    -- ^ (Register bank, Load reg addr)
   -> (Value,Instruction)  -- ^ (Memory output, Current instruction)
   -> ( (Vec 7 Value,Reg)
      , (MemAddr,MemAddr,Bool,Value,InstrAddr)
      )
cpu2 (regbank,ldRegD) (memOut,instr) = ((regbank',ldRegD'),(rdAddr,wrAddr,wrEn,aluOut,fromIntegral ipntr))
  where
    -- Current instruction pointer
    ipntr = regbank !! PC
    -- Decoder
    (MachCode {..}) = case instr of
      Compute op rx ry res -> nullCode {inputX=rx,inputY=ry,result=res,aluCode=op}
      Branch cr a          -> nullCode {inputX=cr,jmpM=Just a}
      Jump a               -> nullCode {aluCode=Incr,jmpM=Just a}
      Load a r             -> nullCode {ldReg=r,rdAddr=a}
      Store r a            -> nullCode {inputX=r,wrAddr=a,wrEn=True}
      Nop                  -> nullCode
    -- ALU
    regX   = regbank !! inputX
    regY   = regbank !! inputY
    aluOut = alu aluCode regX regY
    -- next instruction
    nextPC = case jmpM of
               Just a | aluOut /= 0 -> ipntr + a
               _                    -> ipntr + 1
    -- update registers
    ldRegD'  = ldReg -- Delay the ldReg by 1 cycle
    regbank' = replace Zero   0
             $ replace PC     nextPC
             $ replace result aluOut
             $ replace ldRegD memOut
             $ regbank

system3 :: KnownNat n => Vec n Instruction -> Signal Value
system3 instrs = memOut
  where
    memOut = blockRam (replicate d32 0) wrAddr rdAddr wrEn aluOut
    (rdAddr,wrAddr,wrEn,aluOut,ipntr) = mealyB cpu2 ((replicate d7 0),Zero) (memOut,instr)
    instr  = asyncRom instrs <$> ipntr

prog2 = -- 0 := 4
      Compute Incr Zero RegA RegA :>
      replicate d23 (Compute Incr RegA Zero RegA) ++ -- Change the first value by changing the dNum
      Store RegA 0 :>
      -- 1 := 6
      Compute Incr Zero RegA RegA :>
      replicate d11 (Compute Incr RegA Zero RegA) ++ -- Change the second value by changing the dNum
      Store RegA 1 :>
      -- A := 4
      Load 0 RegA :>
      -- B := 6
      Load 1 RegB :>
      Nop :> -- Extra NOP
      -- start
      Compute CmpGt RegA RegB RegC :>
      Branch RegC 4 :>
      Compute CmpGt RegB RegA RegC :>
      Branch RegC 4 :>
      Jump 5 :>
      -- (a > b)
      Compute Sub RegA RegB RegA :>
      Jump (-6) :>
      -- (b > a)
      Compute Sub RegB RegA RegB :>
      Jump (-8) :>
      -- end
      Store RegA 2 :>
      Load 2 RegC :>
      Nil
--------------------------------------------------------------------------------
