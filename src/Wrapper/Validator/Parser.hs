{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Parser where

import Data.Aeson
import Data.Maybe
import Data.ByteString.Lazy
import Data.Map

data PSettings = PSettings { inputData     :: String,
                             graphType     :: String, 
                             title         :: Maybe String,
                             properties    :: Maybe (Map String String),
                             outputType    :: Maybe String } 
                             deriving Show

instance FromJSON PSettings where
  parseJSON (Object v) =
    PSettings <$> v .:  "inputData"
              <*> v .:  "graphType" 
              <*> v .:? "title"
              <*> v .:? "properties"
              <*> v .:? "outputType"
  parseJSON _ = error "Wrong structure of input file"                               

--Parses a JSON file from a directory on your computer  
main = do b <- Data.ByteString.Lazy.readFile "linePlotTest.json"
          return (decode b :: Maybe PSettings)