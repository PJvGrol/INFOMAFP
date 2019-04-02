module Program where

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
    print opts