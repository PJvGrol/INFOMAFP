{-# Language BlockArguments #-}

module TestValidator where

import Test.HUnit
import Validator
import Parser
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import ErrorData

main = do runTestTT tests

{- Function that parses a file into a list of error messages, that can then be compared. -}
parseFile b = case (decode b :: Maybe PSettings) of Nothing -> Errors [RequiredFieldMissing "One of the required fields has incorrect input or is missing."]
                                                    Just v  -> case parse v of Left e -> e
                                                                               _      -> Errors [NoErrorsFound "Found no errors"]
                                                                               
parseXML' b = case parseXML b of Nothing -> Errors [RequiredFieldMissing "One of the required fields has incorrect input or is missing."]
                                 Just v -> case parse v of Left e -> e 
                                                           _      -> Errors [NoErrorsFound "Found no errors"]
                                                                               
{- All tests in this file bundled together -}
tests = TestList [TestLabel "General tests" general_tests,
                  TestLabel "Line plot tests" lineplot_tests,
                  TestLabel "XML tests" xml_tests]
                                                                                                                                                            
general_tests  = TestList [TestLabel "Test empty" test_empty, 
                           TestLabel "Test missing" test_missing,
                           TestLabel "Test unrecognized" test_unrec
                           ]   
                         
xml_tests      = TestList [TestLabel "Test lines" xml_correct,
                           TestLabel "Test empty" xml_empty,
                           TestLabel "Test missing" xml_missing
                           ]
                         
lineplot_tests = TestList [TestLabel "Test line plot mult. errors" line_mult,
                           TestLabel "Test unrecognized vals line plot" lines_val,
                           TestLabel "Test correct file" line_correct
                            ]
 
---------- General tests -----------                           

{- Tests the output of an empty file -}
test_empty = TestCase (do b <- B.readFile "general/empty.json"
                          assertEqual "Empty file" (decode b :: Maybe PSettings) Nothing)
                          
{- Tests an input file where one of the mandatory fields is missing. -} 
test_missing = TestCase (do b <- B.readFile "general/missing_graphtype.json"
                            assertEqual "Missing graph type" (decode b :: Maybe PSettings) Nothing)
                            
{- Tests an input file with unrecognized input one of the mandatory fields. 
   (in this case the graph type) -}
test_unrec = TestCase (do b <- B.readFile "general/unrecognized_graphtype.json"
                          assertEqual "Unrecognized graph type" (parseFile b) (Errors [UnknownGraphType "The provided graph type is not recognized."]))
 
                          
---------- Line plot tests -----------
                          
{- Tests an input file with no incorrect entries that is completely filled -}
line_correct = TestCase (do b  <- B.readFile "line_plot/line_correct.json"
                            assertEqual "correct" (parseFile b) (Errors [NoErrorsFound "Found no errors"]))

{- An input file for a line plot with multiple errors. -}
line_mult = TestCase (do b  <- B.readFile "line_plot/linePlotTest.json"
                         assertEqual "mult" (parseFile b) t )
                            where t = Errors [(DataNotMatchesType "The provided limit values for a line plot do not match the correct type. The desired type is [[(x,y)]]."), (UnknownOutPutType "The provided output type is not recognized.")]           

{- Tests an input file that has input values of the wrong type -}                            
lines_val = TestCase ( do b  <- B.readFile "line_plot/lines_val.json"
                          assertEqual "Wrong input values" (parseFile b) (Errors [DataNotMatchesType "The input data does not match the required type. In the case of a line plot the input has to have type [[(x,y)]]"]))         
                          
------------ XML tests ---------------

{- Tests a correct xml file for a line plot with all fields filled -}
xml_correct = TestCase (do b <- readFile "xml/line_full.xml"
                           assertEqual "xml line plot" (parseXML' b) (Errors [NoErrorsFound "Found no errors"]))

{- Test an empty xml file -}
xml_empty = TestCase (do b <- readFile "xml/empty.xml"
                         assertEqual "Empty file" (parseXML' b) (Errors [RequiredFieldMissing "One of the required fields has incorrect input or is missing."]))
 
{- Tests an xml file where entry for the graph type is missing -}
xml_missing = TestCase (do b <- readFile "xml/missing.xml"
                           assertEqual "Missing graph type" (parseXML' b) (Errors [RequiredFieldMissing "One of the required fields has incorrect input or is missing."]))