module Wrapper.Parser.Data where

import Data.Map (Map, fromList)
import Data.ByteString.Lazy

data PSettings = PSettings { inputData     :: String,
                             graphType     :: String, 
                             title         :: Maybe String,
                             properties    :: Maybe (Map String String),
                             outputType    :: Maybe String } 
                             deriving (Show, Eq)

data InputString = JsonString ByteString | XmlString String deriving Show
