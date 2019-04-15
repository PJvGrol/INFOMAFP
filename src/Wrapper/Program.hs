module Wrapper.Program where

import Data.ByteString.Lazy

import Wrapper.ChartData
import Wrapper.Parser.Parser
import Wrapper.Rendering.PlotRendering
import Wrapper.Validator.Validator
import Wrapper.Validator.ErrorData

import Control.Monad.Reader
import Options.Applicative
import Data.Semigroup ((<>))

newtype Options = Options {
    oFileToRead :: String
} deriving (Show)

options :: Parser Options
options = Options <$> 
            strOption ( long "file" 
                        <> short 'f' 
                        <> metavar "FILEPATH" 
                        <> help "File used to render the graph." )

parseOptions :: IO Options
parseOptions = execParser opts
    where
        opts = info (options <**> helper)
            ( fullDesc 
            <> progDesc "Provide a JSON or XML file to transform it into a Graph." 
            <> header "ChartWrapper - It was never easier to render graphs!" )

runProgram :: IO ()
runProgram = do
    opts <- parseOptions
    case opts of
        Options h ->
            do
                b <- Data.ByteString.Lazy.readFile h
                case Wrapper.Parser.Parser.parseJson b of
                    Nothing -> print "Parsing failure"
                    Just r -> check (Wrapper.Validator.Validator.parse r)

check :: Either ErrorList (Settings Double Double) -> IO ()
check (Left x) = print (toSList x)
check (Right x) = do
                    Wrapper.Rendering.PlotRendering.tRender x
                    print "Success" 