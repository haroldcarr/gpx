{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module XSpec where

------------------------------------------------------------------------------
import           Gpx
------------------------------------------------------------------------------
import           Test.Hspec
------------------------------------------------------------------------------

spec :: Spec
spec = do
  jacks        <- runIO (readAndParseGpx "test/Jacks_Peak_UT.gpx")
  iStreet      <- runIO (readAndParseGpx "test/I_street.gpx")
  nowhere      <- runIO (readAndParseGpx "test/Nowhere.gpx")
  tuscaroa     <- runIO (readAndParseGpx "test/Tuscarora_UT.gpx")
  it "jacks"    $ (right jacks   , metersToFeet (right jacks))    `shouldBe`
    ( ("",1578.7,1585.3,1578.6,1964.4,453.1999999999996,446.5999999999997)
    , ("",5179.39896,5201.05224,5179.07088,6444.80352,1486.8585599999988,1465.205279999999) )
  it "iStreet"  $ (right iStreet , metersToFeet (right iStreet))  `shouldBe`
    ( ("",1582.3,1581.5,1581.5,1655.5,123.80000000000132,124.60000000000127)
    , ("",5191.20984,5188.5852,5188.5852,5431.3644,406.16304000000434,408.7876800000042) )
  it "nowhere"  $ (right nowhere , metersToFeet (right nowhere))  `shouldBe`
    ( ("",0.0,2000.0,-500.0,2500.0,4500.0,2500.0)
    , ("",0.0,6561.6,-1640.4,8202.0,14763.6,8202.0) )
  it "tuscaroa" $ (right tuscaroa, metersToFeet (right tuscaroa)) `shouldBe`
    ( ("",2671.3,2671.4,2671.3,3241.0,616.7999999999984,616.6999999999985)
    , ("",8764.001040000001,8764.32912,8764.001040000001,10633.0728,2023.5974399999948,2023.269359999995) )
 where
  right = \case
    Left e  -> error (show e)
    Right r -> r
