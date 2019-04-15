{- This module has several functions that help convert PSettings to subtypes 
   defined in ChartData.hs. In this case for all line plot graphs. -}
module Wrapper.Validator.LinesValidator where

import Prelude hiding (lookup)
import Wrapper.ChartData
import Wrapper.Validator.ErrorData
import Text.Read (readMaybe)
import Data.Map
import Wrapper.Validator.ConvertDefaults
import Data.Maybe (fromJust)

{- Fills the required data fields for a line plot. It returns either the data or 
   a list of all errors found. -}
parseLineInput :: String -> Maybe (Map String String) -> Either ErrorList (InputData Double Double) 
parseLineInput inp pro = case parseVal inp of Left e1  -> case lookUpLimit pro of Left e2 -> Left (Errors [e1,e2])
                                                                                  _       -> Left (Errors [e1])
                                              Right v1 -> case lookUpLimit pro of Left e2  -> Left (Errors [e2])
                                                                                  Right v2 -> Right (LinesData (LinesData' v1 v2))

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
parseLimit lim = case readMaybe lim :: Maybe [[(Double,Double)]] of 
                        Just v  -> Right (Just v)
                        Nothing -> Left (DataNotMatchesType "The provided limit values for a line plot do not match the correct type. The desired type is [[(x,y)]].")                                                                       
                                                                          
parseLineProp :: Maybe (Map String String) -> Either ErrorList (Maybe [PropertyType])
parseLineProp dic | dic == Nothing = Right Nothing
                  | otherwise      = case parseLineStyle (fromJust dic) of Right Nothing  -> Right Nothing
                                                                           Right (Just v) -> Right (Just [LineStyle v])
                                                                           Left e         -> Left e
                                                                                                   
parseLineStyle :: (Map String String) -> Either ErrorList (Maybe LineStyle)
parseLineStyle dic = let v1   = parseDouble "lineWidth" dic
                         v2   = parseColor "lineColor" dic
                         v3   = parseDoubleList "lineDashes" dic in
                                foldLineStyle v1 v2 v3

{- Either combines all the errors it has found so far or returns the result as a
   LineStyle -}
foldLineStyle (Left e1) (Left e2) (Left e3)    = Left (Errors ([e1] ++ [e2] ++ [e3]))
foldLineStyle (Left e1) (Left e2) _            = Left (Errors ([e1] ++ [e2]))
foldLineStyle (Left e1) _         (Left e3)    = Left (Errors ([e1] ++ [e3]))
foldLineStyle _         (Left e2) (Left e3)    = Left (Errors ([e2] ++ [e3]))
foldLineStyle (Left e1) _         _            = Left (Errors [e1])
foldLineStyle _         (Left e2) _            = Left (Errors [e2])
foldLineStyle _         _         (Left e3)    = Left (Errors [e3])
foldLineStyle (Right v1) (Right v2) (Right v3) = Right (Just (LineStyle' v1 v2 v3))
