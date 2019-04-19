module Wrapper.Parser.XmlParser where

import Text.XML.HXT.Parser.XmlParsec
import Text.XML.HXT.DOM.XmlNode hiding (getText)
import Text.XML.HXT.Core hiding (xread, getChildren, getText)
import Data.Map (Map, fromList)
import Wrapper.Parser.Data
import Data.Maybe
    
parseXML :: String -> Maybe PSettings
parseXML b = let x = xread b in
                if null x then Nothing else (ntreeToPsettings.removeEmpty.getChildren) (head x) 

{- Checks if a string consists solely of whitespaces -}
isEmpty :: String -> Bool
isEmpty ls = foldr ((&&).(\x -> x == ' ' || x == '\n' || x == '\t')) True ls

{- Removes all empty text fields from a NTree -} 
removeEmpty :: [NTree XNode] -> [NTree XNode] 
removeEmpty []   = []
removeEmpty (x:xs) = case x of (NTree (XText s) _)    -> if isEmpty s then removeEmpty xs else x : (removeEmpty xs)
                               (NTree (XTag s []) ls) -> (NTree (XTag s []) (removeEmpty ls)) : (removeEmpty xs)
                               _                      -> x : (removeEmpty xs)

getText (NTree (XText s) _) = s   
getLabel s (NTree (XTag label _) _) = label     

{- Finds the right tag corresponding to the input string -}
findTag :: String -> [NTree XNode] -> Maybe [NTree XNode]
findTag s [] = Nothing
findTag s ((NTree (XTag label _) ls):xs) = if (localPart label) == s then Just ls else findTag s xs                          
    
parseProp :: [NTree XNode] -> [(String,String)]
parseProp []                         = []
parseProp ((NTree (XTag l _) ls):xs) = (localPart l, getText (head ls)) : (parseProp xs)

{- Changes the NTree that comes from a XML file into the PSettings type -}
ntreeToPsettings :: [NTree XNode] -> Maybe PSettings
ntreeToPsettings [] = Nothing
ntreeToPsettings t = let x1 = findTag "inputData" t
                         x2 = findTag "graphType" t in
                        if x1 == Nothing then Nothing else
                            if x2 == Nothing then Nothing else Just
                            (PSettings (getText (head (fromJust x1))) 
                                        (getText (head (fromJust x2)))
                                        (case findTag "title" t of Just x -> Just (getText (head x))
                                                                   _      -> Nothing)
                                        (case findTag "properties" t of Just x -> Just (fromList (parseProp x))
                                                                        _      -> Nothing)
                                        (case findTag "outputType" t of Just x -> Just (getText (head x))
                                                                        _      -> Nothing))