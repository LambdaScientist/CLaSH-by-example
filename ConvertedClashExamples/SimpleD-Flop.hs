{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures   #-}
{-# LANGUAGE MagicHash        #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TemplateHaskell  #-}


module SimpleDFlop where

import qualified Prelude as P
import CLaSH.Prelude
import Control.Lens hiding ((:>))

import TestingTools

--inputs
data PIn = PIn { _in_1 :: Bit
               , _clk  :: Bit
               } deriving (Eq)
data St = St { _out_1 :: Bit
             } deriving (Eq)
makeLenses ''St

onTrue :: St -> PIn -> Bool -> St
onTrue st PIn{..} condition = if condition then st{ _out_1 = _in_1 } else st

bnot :: Bit -> Bit
bnot 1 = 0
bnot _ = 1

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
instance PortIn PIn
instance SysState St

data Config = Config { input'  :: PIn
                     , startSt' :: St
                     }
instance Show Config where
  show Config{..} = "Config:\n input = " P.++ show input'
               P.++ "\n startSt = " P.++ show startSt'
instance Transition Config where
  runOneTest = runOneTest'

setupTest :: Config -> Signal St
setupTest (Config pin st) = topEntity' st sPin
  where
    sPin = signal pin

setupAndRun :: [[TestResult]]
setupAndRun = runConfigList setupTest configurationList

instance Show PIn where
  show PIn {..} =
    "PIn\n\t _in_1 = " P.++ show _in_1
    P.++ "\n\t _clk = " P.++ show _clk

instance Show St where
 show St {..} =
        "St\n\t _out_1 = " P.++ show _out_1

configurationList :: [Config]
configurationList = [configOne, configTwo, configThree, configFour]
  where
    startSt    = St 0

    inputOne  = PIn 0 0
    configOne = Config inputOne startSt

    inputTwo  = PIn 0 0
    configTwo = Config inputTwo startSt

    inputThree  = PIn 0 0
    configThree = Config inputThree startSt

    inputFour  = PIn 0 0
    configFour = Config inputFour startSt
