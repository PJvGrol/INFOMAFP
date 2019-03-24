module Validator where 

import Parser
import ErrorData
import ChartData
import LinesValidator

{- Transforms the input of the JSON file in either a list of errors or a into 
   settings that can be convert to a graph. If the graph type is not recognized,
   it will immediately return that error. -}
parse :: PSettings -> Either ErrorList (Settings x y z)
parse p = case chartType p of "lines" -> let (Errors e,s) = parseLinePlot p in 
                                           if  null e then Right s else Left (Errors e)
                              _       -> Left (Errors [UnknownGraphType "The provided graph type is not recognized."])

chartType :: PSettings -> String
chartType (PSettings _ t _ _ _) = t

parseType t = case t of "lines" -> Lines

{- Checks whether the provided output type matches any of the defaults. If no 
   input type is provided then it returns SVG -}
parseOut :: Maybe String -> Either ValError OutputType
parseOut t = case t of Just "svg" -> Right SVG
                       Just "png" -> Right PNG
                       Just "ps"  -> Right PS
                       Nothing    -> Right SVG
                       _          -> Left (UnknownOutPutType "The provided output type is not recognized.")

parseLinePlot :: PSettings -> (ErrorList, Settings x y z)
parseLinePlot (PSettings inp typ tit pro out) = undefined --Settings (parseLineInput inp pro) (parseType typ) tit (parseLineProp pro) (parseOut out)