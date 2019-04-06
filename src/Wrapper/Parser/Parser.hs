{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Parser where

import Data.Aeson
import Data.Maybe
import qualified Data.ByteString.Lazy as B (readFile)
import Data.Map (Map, fromList)
import Text.XML.HXT.Parser.XmlParsec
import Text.XML.HXT.DOM.XmlNode hiding (getText)
import Text.XML.HXT.Core hiding (xread, getChildren, getText)

data PSettings = PSettings { inputData     :: String,
                             graphType     :: String, 
                             title         :: Maybe String,
                             properties    :: Maybe (Map String String),
                             outputType    :: Maybe String } 
                             deriving (Show, Eq)

instance FromJSON PSettings where
  parseJSON (Object v) =
    PSettings <$> v .:  "inputData"
              <*> v .:  "graphType" 
              <*> v .:? "title"
              <*> v .:? "properties"
              <*> v .:? "outputType"
  parseJSON _ = error "Wrong structure of input file"                               

--Parses a JSON file from a directory on your computer  
parJson = do b <- B.readFile "line_correct.json"
             return (decode b :: Maybe PSettings)
 
main = do b <- readFile "test.xml"
          return (parseXML b)
          
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