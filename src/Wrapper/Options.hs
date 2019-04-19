module Wrapper.Options where

import Data.Semigroup ((<>))
import           Options.Applicative

data Options = Options
    { 
        oFileToRead :: String,
        oFileToOutput :: String
    }

options :: Parser Options
options = Options <$> 
            strOption ( long "input" 
                        <> short 'i' 
                        <> metavar "FILEPATH" 
                        <> help "File used to render the graph." )
            <*> strOption (long "output"
                        <> short 'o'
                        <> metavar "FILEPATH" 
                        <> help "File name of the graph.")

parseOptions :: IO Options
parseOptions = execParser opts
    where
        opts = info (options <**> helper)
            ( fullDesc 
            <> progDesc "Provide a JSON or XML file to transform it into a Graph." 
            <> header "ChartWrapper - It was never easier to render graphs!" )