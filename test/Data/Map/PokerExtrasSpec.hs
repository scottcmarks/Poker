{-# LANGUAGE NoImplicitPrelude #-}
module Data.Map.PokerExtrasSpec  where

import           Data.Map
import           Data.Map.PokerExtras
import           RIO
import           Test.Hspec

spec :: Spec
spec =  describe "Data Map PokerExtras" $ do
      it "composition and inverse" $ do
          inv ( a ⊚ b ) `shouldBe` inv ( b ) ⊚  inv ( a )

      it "repeated inverse" $ do
          inv ( inv ( c ) )         `shouldNotBe` c
          inv ( inv ( inv ( c ) ) ) `shouldBe`    inv ( c )
  where
      a, b, c :: Map Int Int
      a = fromList[(1,2),(2,3),(3,5),(4,7),(5,11)]
      b = fromList[(2,4),(3,9),(5,25),(7,49),(11,121)]
      c = fromList[(1,2),(2,3),(3,5),(4,7),(5,11),(6,2)] -- not bijective
