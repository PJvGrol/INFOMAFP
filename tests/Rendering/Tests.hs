module Rendering.Tests where

import Wrapper.ChartData

import Test.Tasty.QuickCheck

instance (Arbitary x y) => Arbitrary (Settings x y) where
    arbitary = do
        graphType <- arbitary
        title <- arbitary
        outputType <- arbitary    
        return Settings ()


instance Arbitary GraphType where
    arbitary = oneof [Lines]

instance Arbitary OutputType where
    arbitrary = oneof [SVG ..]

-- instance (Arbitary x y) => Arbitary (LinesData' x y) where
--     arbitary = do
--         values <- arbitary
--         return LinesData' values Nothing