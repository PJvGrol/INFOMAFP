{-# LANGUAGE BlockArguments #-}

module Validator where 

import Parser
import ErrorData
import ChartData
import LinesValidator
import Data.ByteString.Lazy
import Data.Aeson

{- For now, this function reads in a JSON file and returns all the errors that it has found.
   If no errors were found, it returns "Found no errors" -}
main = do b  <- Data.ByteString.Lazy.readFile "linePlotTest.json"
          return (toSList (case (decode b :: Maybe PSettings ) of Nothing -> Errors [RequiredFieldMissing "One of the required fields has incorrect input or is missing."]
                                                                  Just v  -> case parse v of Left e  -> e
                                                                                             Right _ -> Errors [NoErrorsFound "Found no errors"]))
          
{- Transforms the input of the JSON file in either a list of errors or a into 
   settings that can be convert to a graph. If the graph type is not recognized,
   it will immediately return that error. -}
parse :: PSettings -> Either ErrorList (Settings Double Double z)
parse p = case chartType p of "lines" -> parseLinePlot p
                              _       -> Left (Errors [UnknownGraphType "The provided graph type is not recognized."])

chartType :: PSettings -> String
chartType (PSettings _ t _ _ _) = t

parseType t = case t of "lines" -> Lines

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
parseLinePlot :: PSettings -> Either ErrorList (Settings Double Double z)
parseLinePlot (PSettings inp typ tit pro out) = 
     let inp' = parseLineInput inp pro
         typ' = parseType typ
         out' = parseOut out
         pro' = parseLineProp pro in 
            case inp' of Left e1  -> case out' of Left e2 -> case pro' of Left e3   -> Left (combine e1 (combine e2 e3))
                                                                          _         -> Left (combine e1 e2)
                                                  _       -> Left e1
                         Right v1 -> case out' of Left e2  -> case pro' of Left e3  -> Left (combine e2 e3)
                                                                           _        -> Left e2
                                                  Right v2 -> case pro' of Left e3        -> Left e3
                                                                           Right Nothing  -> Right (Settings v1 typ' tit [] v2)
                                                                           Right (Just l) -> Right (Settings v1 typ' tit l v2)

