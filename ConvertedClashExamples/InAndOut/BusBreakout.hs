{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE NoImplicitPrelude  #-}

module InAndOut.BusBreakout where

import CLaSH.Prelude
import Control.Lens hiding ((:>))
import Control.Monad.Trans.State

import Text.PrettyPrint.HughesPJClass

--------------------------------------------------------------------------------
-- Remove this when test is turned into a library
--------------------------------------------------------------------------------

showT :: (Show s) => s -> Doc
showT = text.show

--------------------------------------------------------------------------------

--inputs
data PIn = PIn { _in1 :: BitVector 4
               , _in2 :: BitVector 4
               , _in3 :: Bit
               } deriving (Eq, Show)
instance Pretty PIn where
  pPrint PIn {..} = text "PIn:"
                $+$ text "_in1 =" <+> showT _in1
                $+$ text "_in2 =" <+> showT _in2
                $+$ text "_in3 =" <+> showT _in3

--Outputs and state data
data St = St { _out1 :: BitVector 6
             } deriving (Eq, Show)

makeLenses ''St
instance Pretty St where
 pPrint St {..} = text "St"
              $+$ text "_cntEn ="   <+>  showT _out1


procSimple :: St -> PIn -> St
procSimple st@St{..} PIn{..} = flip execState st $
  out1 .= (p1 ++# p2 ++# p3 ++# p4)
  where
    p1 = slice d3 d2 _in2
    p2 = _in1 ! 3 .&. _in2 ! 1
    p3 = _in1 ! 2 .&. _in2 ! 0
    p4 = slice d1 d0 _in1


topEntity :: Signal PIn -> Signal St
topEntity = topEntity' st
  where
    st = St 0

topEntity' :: St ->  Signal PIn -> Signal St--Signal st
topEntity' st pin = reg
  where
    reg = register st (procSimple <$> reg <*> pin)



---TESTING
--
-- configList :: [Config]
-- configList = [configOne, configTwo, configThree, configFour]
