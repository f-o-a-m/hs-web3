{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
module Network.Ethereum.Contract.Test.THSpec where


import Test.Hspec
import           Network.Ethereum.Contract.TH


-- Contract with Tuples
[abiFrom|test-support/abis/Exchange.json|]

spec :: Spec
spec = 
  describe "quasi-quoter" $ 
    it "can compile contract with tuples" $ 
      True `shouldBe` True
    


