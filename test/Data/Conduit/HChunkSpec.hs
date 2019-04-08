{-# LANGUAGE NoImplicitPrelude #-}
module Data.Conduit.HChunkSpec  where


import           Conduit
import           Data.Conduit.HChunk
import           Data.Conduit.List   (sourceList)
import           Data.List.Ordered
import           RIO
import           Test.Hspec
-- import           Test.Hspec.QuickCheck
-- import           Data.List.Class

spec :: Spec
spec =  describe "Data Conduit HChunk" $ do
    it "prime-headed chunks"
        $ runConduitPure
            (sourceList [0..100] .| takeHChunksC isP100 .| mapC hToList .| sinkList)
            `shouldBe`
            [[0,1]
            ,[2]
            ,[3,4]
            ,[5,6]
            ,[7,8,9,10]
            ,[11,12]
            ,[13,14,15,16]
            ,[17,18]
            ,[19,20,21,22]
            ,[23,24,25,26,27,28]
            ,[29,30]
            ,[31,32,33,34,35,36]
            ,[37,38,39,40]
            ,[41,42]
            ,[43,44,45,46]
            ,[47,48,49,50,51,52]
            ,[53,54,55,56,57,58]
            ,[59,60]
            ,[61,62,63,64,65,66]
            ,[67,68,69,70]
            ,[71,72]
            ,[73,74,75,76,77,78]
            ,[79,80,81,82]
            ,[83,84,85,86,87,88]
            ,[89,90,91,92,93,94,95,96]
            ,[97,98,99,100]]
      where isP100 n = n < 100 && n `elem` p100
            p100 = takeWhile (<100) primesLME :: [Int]
            primesLME = 2 : fix ((3:) . minus [5,7..]
                                      . joinL
                                      . map (\p-> [p*p, p*p+2*p..]))
              where joinL ((x:xs):t) = x : union xs (joinL t)
                    joinL []         = []
                    joinL ([]:t)     = joinL t
            hToList (HChunk h b) = maybe b (:b) h
