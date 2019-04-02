module Main where

import Data.ByteString.Lazy

import Wrapper.ChartData
import Wrapper.Parser.Parser
import Wrapper.Rendering.PlotRendering
import Wrapper.Validator.Validator
import Wrapper.Validator.ErrorData

main = do
        b <- Data.ByteString.Lazy.readFile "linePlotTest.json"
        case Wrapper.Parser.Parser.parseJson b of
            Nothing -> print ("Parsing failure")
            Just r -> check (Wrapper.Validator.Validator.parse r)

            
            
validate Nothing = Nothing
validate (Just s) = Just (Wrapper.Rendering.PlotRendering.render s)

check :: Either ErrorList (Settings Double Double) -> IO ()
check (Left x) = print (toSList x)
check (Right x) = do
                    Wrapper.Rendering.PlotRendering.tRender x
                    print "Success"