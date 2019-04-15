module Wrapper.Validator.Validator where 

import Wrapper.Parser.Parser
import Wrapper.Validator.ErrorData
import Wrapper.ChartData
import Wrapper.Validator.LinesValidator
import Wrapper.Validator.PointsValidator
import Wrapper.Validator.PieValidator
import Wrapper.Validator.BarsValidator
import Data.ByteString.Lazy
import Data.Aeson

{- Transforms the input of the JSON file in either a list of errors or a into 
   settings that can be convert to a graph. If the graph type is not recognized,
   it will immediately return that error. -}
parse :: PSettings -> Either ErrorList (Settings Double Double)
parse p = case chartType p of "lines"  -> parseLinePlot p
                              "points" -> parsePoints p
                              "bars"   -> parseBars p
                              "pie"    -> parsePie p
                              _        -> Left (Errors [UnknownGraphType "The provided graph type is not recognized."])
                              

chartType :: PSettings -> String
chartType (PSettings _ t _ _ _) = t

parseType t = case t of "lines"  -> Lines
                        "points" -> Points
                        "bars"   -> Bars
                        "pie"    -> Pie

{- Checks whether the provided output type matches any of the defaults. If no 
   input type is provided then it returns SVG -}
parseOut :: Maybe String -> Either ErrorList OutputType
parseOut t = case t of Just "svg" -> Right SVG
                       Just "png" -> Right PNG
                       Just "ps"  -> Right PS
                       Nothing    -> Right SVG
                       _          -> Left (Errors [UnknownOutPutType "The provided output type is not recognized."])

{- The main function that parses a line plot. It combines all the results or errors 
   that have been found during parsing. -}
parseLinePlot :: PSettings -> Either ErrorList (Settings Double Double)
parseLinePlot ps = parsePlot ps parseLineInput parseLineProp

parsePoints :: PSettings -> Either ErrorList (Settings Double Double) 
parsePoints ps = parsePlot ps parsePointInput parsePointProp

parseBars :: PSettings -> Either ErrorList (Settings Double Double)
parseBars ps = parsePlot ps parseBarsInput parseBarsProp

parsePie :: PSettings -> Either ErrorList (Settings Double Double)
parsePie ps = parsePlot ps parsePieInput parsePieProp

{- A function that works for any type of plot, provided with a function to parse 
   the input values and a function that parses the properties. -}
parsePlot (PSettings inp typ tit pro out) f_inp f_prop = 
     let inp' = f_inp inp pro
         typ' = parseType typ
         out' = parseOut out
         pro' = f_prop pro in 
            case inp' of Left e1  -> case out' of Left e2 -> case pro' of Left e3   -> Left (combine e1 (combine e2 e3))
                                                                          _         -> Left (combine e1 e2)
                                                  _       -> Left e1
                         Right v1 -> case out' of Left e2  -> case pro' of Left e3  -> Left (combine e2 e3)
                                                                           _        -> Left e2
                                                  Right v2 -> case pro' of Left e3        -> Left e3
                                                                           Right Nothing  -> Right (Settings v1 typ' tit [] v2)
                                                                           Right (Just l) -> Right (Settings v1 typ' tit l v2)
