module Validator.Tests (testCollection) where

import Test.Tasty.HUnit
import Wrapper.Validator.FileValidator
import Wrapper.Parser.JsonParser
import Wrapper.Parser.XmlParser
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import Wrapper.ErrorHandling
import Wrapper.Parser.Data

{- Function that parses a file into a list of error messages, that can then be compared. -}
parseFile b = case (decode b :: Maybe PSettings) of Nothing -> Errors [RequiredFieldMissing "One of the required fields has incorrect input or is missing."]
                                                    Just v  -> case validate v of Left e -> e
                                                                                  _      -> Errors [NoErrorsFound "Found no errors"]
                                                                               
parseXML' b = case parseXML b of Nothing -> Errors [RequiredFieldMissing "One of the required fields has incorrect input or is missing."]
                                 Just v -> case validate v of Left e -> e 
                                                              _      -> Errors [NoErrorsFound "Found no errors"]
                 
file :: String -> String
file testFilePath = "./tests/Validator/" ++ testFilePath
                                                                               
{- All tests in this file bundled together -}
-- runTests = TestList [TestLabel "General tests" general_tests,
--                   TestLabel "Line plot tests" lineplot_tests,
--                   TestLabel "XML tests" xml_tests,
--                   TestLabel "Point plot tests" pointplot_tests,
--                   TestLabel "Bars plot tests" bars_tests,
--                   TestLabel "Pie plot tests" pie_tests]

testCollection = [
        test_empty,
        test_missing,
        test_unrec
    ]
                                                                                                                                                                                      
---------- General tests -----------                           

{- Tests the output of an empty file -}
test_empty = testCase "Test Empty" $ do 
    b <- B.readFile "tests/Validator/General/empty.json"
    assertEqual "Empty file" (decode b :: Maybe PSettings) Nothing
                          
-- {- Tests an input file where one of the mandatory fields is missing. -} 
test_missing = testCase "Test Missing" $ do 
    b <- B.readFile "tests/Validator/General/missing_graphtype.json"
    assertEqual "Missing graph type" (decode b :: Maybe PSettings) Nothing
                            
-- {- Tests an input file with unrecognized input one of the mandatory fields. 
--    (in this case the graph type) -}
test_unrec = testCase "Test Unrecognized" $ do 
    b <- B.readFile $ file "General/unrecognized_graphtype.json"
    assertEqual "Unrecognized graph type" (parseFile b) (Errors [UnknownGraphType "The provided graph type is not recognized."])
                          
-- ---------- Line plot tests -----------
                          
-- {- Tests an input file with no incorrect entries that is completely filled -}
-- line_correct = testCase (do b  <- B.readFile "line_plot/line_correct.json"
--                             assertEqual "correct" (parseFile b) (Errors [NoErrorsFound "Found no errors"]))

-- {- An input file for a line plot with multiple errors. -}
-- line_mult = testCase (do b  <- B.readFile "line_plot/linePlotTest.json"
--                          assertEqual "mult" (parseFile b) t )
--                             where t = Errors [(DataNotMatchesType "The provided limit values for a line plot do not match the correct type. The desired type is [[(x,y)]]."), (UnknownOutPutType "The provided output type is not recognized.")]           

-- {- Tests an input file that has input values of the wrong type -}                            
-- lines_val = testCase ( do b  <- B.readFile "line_plot/lines_val.json"
--                           assertEqual "Wrong input values" (parseFile b) (Errors [DataNotMatchesType "The input data does not match the required type. In the case of a line plot the input has to have type [[(x,y)]]"]))         
                          
-- ------------ XML tests ---------------

-- {- Tests a correct xml file for a line plot with all fields filled -}
-- xml_correct = testCase (do b <- readFile "xml/line_full.xml"
--                            assertEqual "xml line plot" (parseXML' b) (Errors [NoErrorsFound "Found no errors"]))

-- {- Test an empty xml file -}
-- xml_empty = testCase (do b <- readFile "xml/empty.xml"
--                          assertEqual "Empty file" (parseXML' b) (Errors [RequiredFieldMissing "One of the required fields has incorrect input or is missing."]))
 
-- {- Tests an xml file where entry for the graph type is missing -}
-- xml_missing = testCase (do b <- readFile "xml/missing.xml"
--                            assertEqual "Missing graph type" (parseXML' b) (Errors [RequiredFieldMissing "One of the required fields has incorrect input or is missing."]))
                           
-- --------- Point plot tests ------------

-- {- Tests an input file with no incorrect entries that is completely filled -}
-- points_correct = testCase (do b  <- B.readFile "point_plot/points_correct.json"
--                               assertEqual "correct" (parseFile b) (Errors [NoErrorsFound "Found no errors"]))
                              
-- {- Tests an input file that has input values of the wrong type -}                            
-- points_val = testCase ( do b  <- B.readFile "point_plot/points_val.json"
--                            assertEqual "Wrong input values" (parseFile b) (Errors [DataNotMatchesType "The input data does not match the required type. In the case of a point plot the input has to have type [(x,y)]."])) 
                           
-- {- Tests a correct xml file for a line plot with all fields filled -}
-- points_xml = testCase (do b <- readFile "point_plot/points_xml.xml"
--                           assertEqual "xml point plot" (parseXML' b) (Errors [NoErrorsFound "Found no errors"]))
                          
-- ---------- Bars plot tests -------------

-- {- Tests a valid input file with all fields filled -} 
-- bars_correct = testCase (do b  <- B.readFile "bars_plot/bars_correct.json"
--                             assertEqual "correct" (parseFile b) (Errors [NoErrorsFound "Found no errors"]))
 
-- {- Test a bars plot with 3 different errors in it -}                            
-- bars_mul = testCase (do b <- B.readFile "bars_plot/bars_mul.json"
--                         assertEqual "correct" (parseFile b) (Errors [DataNotMatchesType "The input data does not match the required type. In the case of a bars plot the input has to have type [(x,[y])]",DataNotMatchesType "The provided value item_styles has an unknown type.  should have type [Color].",DataNotMatchesType "The provided value reference has an unknown type.  should have type Double."]))
                        
-- ----------- Pie plot tests -------------  

-- {- Tests a correct json file of a pie chart -}
-- pie_correct = testCase (do b  <- B.readFile "pie_plot/pie_correct.json"
--                            assertEqual "correct" (parseFile b) (Errors [NoErrorsFound "Found no errors"]))
                           
-- pie_errors = testCase (do b  <- B.readFile "pie_plot/pie_errors.json"
--                           assertEqual "erros" (parseFile b) (Errors [DataNotMatchesType "A Piechart has to have input values of type [(String, Maybe Color, Double, Double)], namely (label, color, offset, value)."]))
                                            