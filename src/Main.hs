module Main where

import qualified Parser.JsonParser as JsonParser
import Parser.ParserData

main :: IO ()
main = putStrLn "Hello world"
-- main :: IO ()
-- main = do 
--     pSettings <- parse "Test.hs"
--     return ()

-- parse :: FilePath -> IO ()
-- parse path = case takeExtension path of
--     ".json" -> JsonParser.parse file
--     _ -> print "Unable to parse file extension"
--     where
--         file = readFile path