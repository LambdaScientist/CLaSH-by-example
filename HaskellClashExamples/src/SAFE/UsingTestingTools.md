--------------------------------------------------------------------------------
-- Stuff to define for each file that wants to use the testing tools
--------------------------------------------------------------------------------

instance PortIn PIn
instance SysState St

data Config = Config { input'  :: PIn
                     , startSt' :: St
                     }
instance Pretty Config where
 pPrint Config{..} = text "Config:"
                 $+$ text "input ="   <+> pPrint input
                 $+$ text "startSt =" <+>  pPrint startSt
instance  Transition Config where
  runOneTest = runOneTest'

setupTest :: Config -> Signal St
setupTest (Config pin st) = topEntity' st sPin
  where
    sPin = signal pin

setupAndRun :: [[TestResult]]
setupAndRun = runConfigList setupTest configurationList

--Probably no copy pasta here
instance Pretty PIn where
instance Pretty St where
configurationList :: [Config]
