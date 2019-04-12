{- This module has several functions that help convert PSettings to subtypes 
   defined in ChartData.hs. In this case for all bars plot graphs. -}
module BarsValidator where

import ErrorData
import ChartData
import Text.Read (readMaybe)
import Data.Map (Map)
import Data.Maybe (fromJust)
import ConvertDefaults 
         
parseBarsInput :: String -> Maybe (Map String String) -> Either ErrorList (InputData Double Double)
parseBarsInput s dic = let vals = parseVal s in
                         case dic of Nothing   -> foldErrors vals (Right Nothing) (Right Nothing) (Right Nothing) (Right Nothing) 
                                     Just dic' -> let v1 = parseColorList "item_styles" dic'
                                                      v2 = parseStringList "titles" dic'
                                                      v3 = parseDouble "reference" dic'
                                                      v4 = parseDouble "singleton_width" dic'
                                                      in foldErrors vals v1 v2 v3 v4
                                          
{- Tries to parse the input values for a line plot. -}                                                                                  
parseVal :: String -> Either ValError [(Double,[Double])]
parseVal inp = case readMaybe inp :: Maybe [(Double,[Double])] of Just v  -> Right v
                                                                  Nothing -> Left (DataNotMatchesType "The input data does not match the required type. In the case of a bars plot the input has to have type [(x,[y])]")
 
-- There are no properties for a bars plot, they are already collected in the values,
-- so this function always succeeds.  
parseBarsProp :: Maybe (Map String String) -> Either ErrorList (Maybe [PropertyType])
parseBarsProp dic = Right Nothing 

{- This is not the most elegant function, but after trying many other things I could not 
   come up with a better solution. -}
foldErrors (Left e1) (Left e2) (Left e3) (Left e4) (Left e5) = Left (Errors ([e1] ++ [e2] ++ [e3] ++ [e4] ++ [e5]))  
foldErrors _         (Left e2) (Left e3) (Left e4) (Left e5) = Left (Errors ([e2] ++ [e3] ++ [e4] ++ [e5])) 
foldErrors (Left e1) _         (Left e3) (Left e4) (Left e5) = Left (Errors ([e1] ++ [e3] ++ [e4] ++ [e5]))  
foldErrors (Left e1) (Left e2) _         (Left e4) (Left e5) = Left (Errors ([e1] ++ [e2] ++ [e4] ++ [e5]))  
foldErrors (Left e1) (Left e2) (Left e3) _         (Left e5) = Left (Errors ([e1] ++ [e2] ++ [e3] ++ [e5]))  
foldErrors (Left e1) (Left e2) (Left e3) (Left e4) _         = Left (Errors ([e1] ++ [e2] ++ [e3] ++ [e4]))  
foldErrors _         _         (Left e3) (Left e4) (Left e5) = Left (Errors ([e3] ++ [e4] ++ [e5]))         
foldErrors _         (Left e2) _         (Left e4) (Left e5) = Left (Errors ([e2] ++ [e4] ++ [e5]))
foldErrors _         (Left e2) (Left e3) _         (Left e5) = Left (Errors ([e2] ++ [e3] ++ [e5]))
foldErrors _         (Left e2) (Left e3) (Left e4) _         = Left (Errors ([e2] ++ [e3] ++ [e4]))
foldErrors (Left e1) _         _         (Left e4) (Left e5) = Left (Errors ([e1] ++ [e4] ++ [e5]))
foldErrors (Left e1) _         (Left e3) _         (Left e5) = Left (Errors ([e1] ++ [e3] ++ [e5]))
foldErrors (Left e1) _         (Left e3) (Left e4) _         = Left (Errors ([e1] ++ [e3] ++ [e4]))
foldErrors (Left e1) (Left e2) _         _         (Left e5) = Left (Errors ([e1] ++ [e2] ++ [e5]))
foldErrors (Left e1) (Left e2) _         (Left e4) _         = Left (Errors ([e1] ++ [e2] ++ [e4]))
foldErrors (Left e1) (Left e2) (Left e3) _         _         = Left (Errors ([e1] ++ [e2] ++ [e3]))
foldErrors _         _         _         (Left e4) (Left e5) = Left (Errors ([e4] ++ [e5])) 
foldErrors _         _         (Left e3) _         (Left e5) = Left (Errors ([e3] ++ [e5])) 
foldErrors _         _         (Left e3) (Left e4) _         = Left (Errors ([e3] ++ [e4])) 
foldErrors _         (Left e2) _         _         (Left e5) = Left (Errors ([e2] ++ [e5]))
foldErrors _         (Left e2) _         (Left e4) _         = Left (Errors ([e2] ++ [e4]))
foldErrors _         (Left e2) (Left e3) _         _         = Left (Errors ([e2] ++ [e3]))
foldErrors (Left e1) _         _         _         (Left e5) = Left (Errors ([e1] ++ [e5]))
foldErrors (Left e1) _         _         (Left e4) _         = Left (Errors ([e1] ++ [e4]))
foldErrors (Left e1) _         (Left e3) _         _         = Left (Errors ([e1] ++ [e3]))
foldErrors (Left e1) (Left e2) _         _         _         = Left (Errors ([e1] ++ [e2]))
foldErrors (Left e1) _         _         _         _         = Left (Errors ([e1]))
foldErrors _         (Left e2) _         _         _         = Left (Errors ([e2]))
foldErrors _         _         (Left e3) _         _         = Left (Errors ([e3]))
foldErrors _         _         _         (Left e4) _         = Left (Errors ([e4]))
foldErrors _         _         _         _         (Left e5) = Left (Errors ([e5]))
foldErrors (Right v1) (Right v2) (Right v3) (Right v4) (Right v5) = Right (BarsData (BarsData' v1 v2 v3 v4 v5))
                          
                                                                  
                                                                  
                                                                  
                                                                  
                                                                  
