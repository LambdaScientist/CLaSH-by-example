{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TemplateHaskell  #-}

module ClocksAndRegisters.SimpleDFlop where

import CLaSH.Prelude

import qualified Prelude as P
import Control.Lens hiding ((:>))

import Text.PrettyPrint.HughesPJClass
--------------------------------------------------------------------------------
-- Remove this when test is turned into a library
--------------------------------------------------------------------------------

showT :: (Show s) => s -> Doc
showT = text.show

--------------------------------------------------------------------------------

--inputs
data PIn = PIn { _in_1 :: Bit
               , _clk  :: Bit
               } deriving (Eq)
instance Pretty PIn where
  pPrint PIn {..} = text "PIn:"
                $+$ text "_in_1 =" <+> showT _in_1
                $+$ text "_clk ="  <+> showT _clk

data St = St { _out_1 :: Bit
             } deriving (Eq)
makeLenses ''St
instance Pretty St where
 pPrint St {..} = text "St"
              $+$ text "_out_1 =" <+> showT _out_1

onTrue :: St -> PIn -> Bool -> St
onTrue st PIn{..} condition = if condition then st{ _out_1 = _in_1 } else st

topEntity :: Signal PIn -> Signal St
topEntity = topEntity' startSt
  where
    startSt = St 0

topEntity' :: St -> Signal PIn -> Signal St
topEntity' st pin = result
  where
    result = register st (onTrue <$> result <*> pin <*> rising )
    rising = isRising 0 clk
    clk = _clk <$> pin

---TESTING
instance Show PIn where
  show PIn {..} =
    "PIn\n\t _in_1 = " P.++ show _in_1
    P.++ "\n\t _clk = " P.++ show _clk

instance Show St where
 show St {..} =
        "St\n\t _out_1 = " P.++ show _out_1
