{-# LANGUAGE OverloadedStrings #-}

module Parser.JsonParser where

import Data.Aeson
import Data.Maybe
import Data.ByteString.Lazy
import Data.Map

import Parser.ParserData

instance FromJSON PSettings where
  parseJSON (Object v) =
    PSettings <$> v .:  "inputData"
              <*> v .:  "graphType" 
              <*> v .:? "title"
              <*> v .:? "properties"
              <*> v .:? "outputType"
  parseJSON _ = error "Wrong structure of input file"                               

parse :: ByteString -> Maybe PSettings
parse = decode