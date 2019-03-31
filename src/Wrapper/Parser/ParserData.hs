module Parser.ParserData where

import Data.Map

data PSettings = PSettings { inputData     :: String,
                             graphType     :: String, 
                             title         :: Maybe String,
                             properties    :: Maybe (Map String String),
                             outputType    :: Maybe String } 
                             deriving (Show, Eq)
