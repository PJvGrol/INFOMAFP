module Wrapper.Parser.Parser where

import Wrapper.Parser.JsonParser
import Wrapper.Parser.XmlParser
import Wrapper.Parser.Data

parse :: InputString -> Maybe PSettings
parse (JsonString b) = parseJson b
parse (XmlString s) = parseXML s