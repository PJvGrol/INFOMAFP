{- This module has several functions that help convert PSettings to subtypes 
   defined in ChartData.hs. In this case for all point plot graphs. -}
module Wrapper.Validator.PointsValidator where

import Wrapper.ErrorHandling
import Wrapper.ChartData
import Wrapper.Validator.ConvertDefaults (parseDouble, parseColor)

import Text.Read (readMaybe)
import Data.Map (Map)
import Data.Maybe (fromJust)

parsePointInput :: String -> Maybe (Map String String) -> Either ErrorList (InputData Double Double)
parsePointInput inp pro = case parseVal inp of Left e  -> Left e
                                               Right v -> Right (PlotPoints v)

{- Tries to parse the input data for a point plot. -}
parseVal :: String -> Either ErrorList [(Double,Double)]
parseVal inp = case readMaybe inp :: Maybe [(Double,Double)] of Just v  -> Right v
                                                                Nothing -> Left (Errors [DataNotMatchesType "The input data does not match the required type. In the case of a point plot the input has to have type [(x,y)]."])

{- Parses all possible properties for a point plot. In this case, the only option
   would be a PointStyle. -}
parsePointProp :: Maybe (Map String String) -> Either ErrorList (Maybe [PropertyType])
parsePointProp dic | dic == Nothing = Right Nothing
                   | otherwise      = case parsePointStyle (fromJust dic) of Right Nothing  -> Right Nothing
                                                                             Right (Just v) -> Right (Just [PointStyle v])
                                                                             Left e         -> Left e                                                                
                                                                
parsePointStyle :: (Map String String) -> Either ErrorList (Maybe PointStyle)
parsePointStyle dic = let v1 = parseColor "color" dic
                          v2 = parseColor "borderColor" dic
                          v3 = parseDouble "borderWidth" dic
                          v4 = parseDouble "pointRadius" dic in
                               foldPointStyle v1 v2 v3 v4

{- Either combines all the errors encountered so far or returns a PointStyle -}
foldPointStyle (Left e1) (Left e2) (Left e3) (Left e4) = Left (Errors ([e1] ++ [e2] ++ [e3] ++ [e4]))
foldPointStyle (Left e1) (Left e2) _         (Left e4) = Left (Errors ([e1] ++ [e2] ++ [e4]))
foldPointStyle (Left e1) _         (Left e3) (Left e4) = Left (Errors ([e1] ++ [e3] ++ [e4]))
foldPointStyle _         (Left e2) (Left e3) (Left e4) = Left (Errors ([e2] ++ [e3] ++ [e4]))
foldPointStyle (Left e1) (Left e2) (Left e3) _         = Left (Errors ([e1] ++ [e2] ++ [e3]))
foldPointStyle (Left e1) (Left e2) _         _         = Left (Errors ([e1] ++ [e2]))
foldPointStyle (Left e1) _         (Left e3) _         = Left (Errors ([e1] ++ [e3]))
foldPointStyle (Left e1) _         _         (Left e4) = Left (Errors ([e1] ++ [e4]))
foldPointStyle _         (Left e2) (Left e3) _         = Left (Errors ([e2] ++ [e3]))
foldPointStyle _         (Left e2) _         (Left e4) = Left (Errors ([e2] ++ [e4]))
foldPointStyle _         _         (Left e3) (Left e4) = Left (Errors ([e3] ++ [e4]))
foldPointStyle (Left e1) _         _         _         = Left (Errors [e1])
foldPointStyle _         (Left e2) _         _         = Left (Errors [e2])
foldPointStyle _         _         (Left e3) _         = Left (Errors [e3])
foldPointStyle _         _         _         (Left e4) = Left (Errors [e4])
foldPointStyle (Right v1) (Right v2) (Right v3) (Right v4) = Right (Just (PointStyle' v1 v2 v3 v4))
                                