{- This module contains some helpful parser functions for several basic data types. -}
module ConvertDefaults where

import ChartData
import Prelude hiding (lookup)
import Data.Map
import ErrorData
import Text.Read (readMaybe)

parseDouble :: String -> (Map String String) -> Either ValError (Maybe Double) 
parseDouble s dic = case lookup s dic of Nothing -> Right Nothing
                                         Just v  -> case readMaybe v :: Maybe Double of 
                                                            Nothing -> Left (DataNotMatchesType ("The provided value " ++ s ++ " has an unknown type. " ++ " should have type Double."))
                                                            v'      -> Right v'
                                                            
parseColor :: String -> (Map String String) -> Either ValError (Maybe Color) 
parseColor s dic = case lookup s dic of Nothing -> Right Nothing
                                        Just v  -> case v of "red"   -> Right (Just Red)
                                                             "black" -> Right (Just Black)
                                                             "blue"  -> Right (Just Blue)
                                                             "white" -> Right (Just White)
                                                             _       -> Left (DataNotMatchesType ("The provided value " ++ s ++ " has an unknown type. " ++ " should have type Color."))
                                                             
parseDoubleList :: String -> (Map String String) -> Either ValError (Maybe [Double])
parseDoubleList s dic = case lookup s dic of Nothing -> Right Nothing
                                             Just v  -> case readMaybe v :: Maybe [Double] of
                                                               Nothing -> Left (DataNotMatchesType ("The provided value " ++ s ++ " has an unknown type. " ++ " should have type Color."))
                                                               v'      -> Right v'