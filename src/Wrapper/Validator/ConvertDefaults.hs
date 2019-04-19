{- This module contains some helpful parser functions for several basic data types. -}
module Wrapper.Validator.ConvertDefaults where

import Wrapper.ChartData
import Wrapper.ErrorHandling

import Prelude hiding (lookup)
import Data.Map hiding (map, foldr)
import Data.Maybe (fromJust)
import Text.Read (readMaybe)

parseDouble :: String -> (Map String String) -> Either ValError (Maybe Double) 
parseDouble s dic = case lookup s dic of Nothing -> Right Nothing
                                         Just v  -> case readMaybe v :: Maybe Double of 
                                                            Nothing -> Left (DataNotMatchesType ("The provided value " ++ s ++ " has an unknown type. " ++ " should have type Double."))
                                                            v'      -> Right v'
  
parseColor' :: String -> Either ValError (Maybe Color)
parseColor' s = case s of "red"   -> Right (Just Red)
                          "black" -> Right (Just Black)
                          "blue"  -> Right (Just Blue)
                          "white" -> Right (Just White)
                          _       -> Left (DataNotMatchesType ("The provided value " ++ s ++ " has an unknown type. " ++ " should have type Color."))
                                                            
parseColor :: String -> (Map String String) -> Either ValError (Maybe Color) 
parseColor s dic = case lookup s dic of Nothing -> Right Nothing
                                        Just v  -> parseColor' v
                                                             
parseDoubleList :: String -> (Map String String) -> Either ValError (Maybe [Double])
parseDoubleList s dic = case lookup s dic of Nothing -> Right Nothing
                                             Just v  -> case readMaybe v :: Maybe [Double] of
                                                               Nothing -> Left (DataNotMatchesType ("The provided value " ++ s ++ " has an unknown type. " ++ " should have type [Double]."))
                                                               v'      -> Right v'

parseStringList :: String -> (Map String String) -> Either ValError (Maybe [String])
parseStringList s dic = case lookup s dic of Nothing -> Right Nothing
                                             Just v  -> case readMaybe v :: Maybe [String] of
                                                             Nothing -> Left (DataNotMatchesType ("The provided value " ++ s ++ " has an unknown type. " ++ " should have type [String]."))
                                                             v'      -> Right v'
                                                               
parseColorList :: String -> (Map String String) -> Either ValError (Maybe [Color])
parseColorList s dic = case lookup s dic of Nothing -> Right Nothing
                                            Just v  -> case readMaybe v :: Maybe [String] of
                                                           Nothing -> Left (DataNotMatchesType ("The provided value " ++ s ++ " has an unknown type. " ++ " should have type [Color]."))
                                                           Just v  -> case colors v of Nothing -> Left (DataNotMatchesType ("The provided value " ++ s ++ " has an unknown type. " ++ " should have type [Color]."))
                                                                                       x       -> Right x 
                                                           
colors :: [String] -> Maybe [Color]
colors l = if foldr ((&&).(\x -> x /= Nothing)) True convertedList then Just (map fromJust convertedList) else Nothing
               where convert "red"   = Just Red
                     convert "black" = Just Black
                     convert "blue"  = Just Blue
                     convert "white" = Just White
                     convert _       = Nothing
                     convertedList = map convert l