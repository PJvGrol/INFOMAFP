{-# LANGUAGE OverloadedStrings #-}

module Wrapper.Parser.JsonParser where

import Data.Aeson
import Data.ByteString.Lazy
import Data.Map (Map, fromList)
import Wrapper.Parser.Data

instance FromJSON PSettings where
    parseJSON (Object v) =
        PSettings <$> v .:  "inputData"
                  <*> v .:  "graphType" 
                  <*> v .:? "title"
                  <*> v .:? "properties"
                  <*> v .:? "outputType"
    parseJSON _ = error "Wrong structure of input file"  

parseJson :: ByteString -> Maybe PSettings
parseJson = decode