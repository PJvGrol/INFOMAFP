module LinesValidator where

import Prelude hiding (lookup)
import Parser
import ChartData
import ErrorData
import Text.Read (readMaybe)
import Data.Map

{- Fills the requiered data fields for a line plot. It returns either the data or 
   a list of all errors found. -}
parseLineInput :: String -> Maybe (Map String String) -> Either ErrorList (LinesData Double Double) 
parseLineInput inp pro = case parseVal inp of Left e1  -> case lookUpLimit pro of Left e2 -> Left (Errors [e1,e2])
                                                                                  _       -> Left (Errors [e1])
                                              Right v1 -> case lookUpLimit pro of Left e2  -> Left (Errors [e2])
                                                                                  Right v2 -> Right (LinesData' v1 v2)

{- Tries to parse the input data for a line plot. -}                                                                                  
parseVal :: String -> Either ValError [[(Double,Double)]]
parseVal inp = case readMaybe inp :: Maybe [[(Double,Double)]] of Just v  -> Right v
                                                                  Nothing -> Left (DataNotMatchesType "The input data does not match the required type. In the case of a line plot the input has to have type [[(x,y)]]")

{- Checks whether there are limit values in the provided list of properties
   and if so it calls parseLimit to check whether the data provided is type correct. -}
lookUpLimit :: Maybe (Map String String) -> Either ValError (Maybe [[(Double,Double)]])
lookUpLimit dic = case dic of Nothing -> Right Nothing 
                              Just d  -> case lookup "limit_values" d of Nothing -> Right Nothing
                                                                         Just x  -> case parseLimit x of Right v -> Right v
                                                                                                         e       -> e
                                                                
parseLimit :: String -> Either ValError (Maybe [[(Double,Double)]])  
parseLimit lim = case readMaybe lim :: Maybe [[(Double,Double)]] of Just v  -> Right (Just v)
                                                                    Nothing -> Left (DataNotMatchesType "The provided limit values for a line plot do not match the correct type. The desired type is [[(Limit x, Limit y)]].")
                                                                       
lookUpLineStyle :: Maybe (Map String String) -> Either ErrorList (Maybe LineStyle)
lookUpLineStyle dic = case dic of Nothing -> Right Nothing
                                  Just d  -> case lookup "line_style" d of Nothing -> Right Nothing
                                                                           Just v  -> parseLineStyle v
                                                                          
parseLineStyle :: String -> Either ErrorList (Maybe LineStyle)
parseLineStyle s = let val = readMaybe s :: Maybe (String,String,String) in 
                     case val of Nothing          -> Left (Errors [DataNotMatchesType "The provided line style should have the form (x,y,z)."])
                                 Just (v1,v2,v3)  -> undefined     
                                 
                     