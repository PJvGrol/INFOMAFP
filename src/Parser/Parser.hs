{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Parser where

import Data.Aeson
import Data.Maybe
import Data.ByteString.Lazy
import Data.Map

data PSettings = PSettings { dataType      :: Text,
                             graphType     :: Text, 
                             title         :: Maybe Text,
                             values        :: Text,
                             properties    :: Maybe [Map Text Text],
                             outputType    :: Maybe Text } 
                             deriving Show

type Text = String

instance FromJSON PSettings where
  parseJSON (Object v) =
    PSettings <$> v .:  "dataType"
              <*> v .:  "graphType" 
              <*> v .:? "title"
              <*> v .:  "values"
              <*> v .:? "properties"
              <*> v .:? "outputType"
  parseJSON _ = error "Wrong structure of input file"                               

--Parses a JSON file from a directory on your computer  
main = do b <- Data.ByteString.Lazy.readFile "jsontest.json"
          return (decode b :: Maybe PSettings)