{- This module has several functions that help convert PSettings to subtypes 
   defined in ChartData.hs. In this case for all pie plot graphs. -}
module Wrapper.Validator.PieValidator where

import Wrapper.ChartData
import Wrapper.ErrorHandling
import Wrapper.Validator.ConvertDefaults

import Text.Read (readMaybe)
import Data.Map

{- Fills the required data fields for a pie plot. It returns either the data or 
   a list of all errors found. -} 
parsePieInput :: String -> Maybe (Map String String) ->Either ErrorList (InputData Double Double)
parsePieInput s _ = case parsePieInput' s of Left e  -> Left e
                                             Right v -> Right (PieData (toPieItem v))

{- Helper function of parsePieInput. Conversion to PieItem is still necessary. -}
parsePieInput' :: String -> Either ErrorList [(String, Maybe Color, Double, Double)]
parsePieInput' s = case readMaybe s :: Maybe ([(String, String, Double, Double)])
                        of Nothing -> Left (Errors [DataNotMatchesType "A Piechart has to have input values of type [(String, Maybe Color, Double, Double)], namely (label, color, offset, value)."])
                           Just v  -> Right (parsePieColor v)
  
{- Converts the strings in pie items to colors. -}                          
parsePieColor :: [(String, String, Double, Double)] -> [(String, Maybe Color, Double, Double)]
parsePieColor (x:xs) = parsePieColor' x : parsePieColor xs

parsePieColor' :: (String, String, Double, Double) -> (String, Maybe Color, Double, Double)
parsePieColor' (v1,v2,v3,v4) = case parseColor' v2 of Left e  -> (v1, Nothing, v3, v4)
                                                      Right c -> (v1, c, v3, v4)
 
{- Changes the (succesfully) parsed input values to pie items. -}
toPieItem :: [(String, Maybe Color, Double, Double)] -> [PieItem]
toPieItem [] = []
toPieItem ((v1,v2,v3,v4):ls) = (PieItem v1 v2 v3 v4) : (toPieItem ls)

{- Parses the pie chart properties such as the background and the margin. -}
parsePieProp :: Maybe (Map String String) -> Either ErrorList (Maybe [PropertyType])
parsePieProp dic = case dic of Nothing   -> Right Nothing
                               Just dic' -> let background_color = parseColor "background" dic' 
                                                margin           = parseDouble "margin" dic'
                                                layout           = parsePieLayout background_color margin in
                                                case layout of Right v -> Right (Just [v])
                                                               Left e  -> Left e
                                         
parsePieLayout (Left e1) (Left e2) = Left (Errors ([e1] ++ [e2]))
parsePieLayout _         (Left e2) = Left (Errors [e2])
parsePieLayout (Left e1) _         = Left (Errors [e1])
parsePieLayout (Right b) (Right m) = Right (PieLayout (PieLayout' b m))


