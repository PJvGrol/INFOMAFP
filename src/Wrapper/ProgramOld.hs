module Wrapper.Program.Old where

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

type AppConfig = MonadReader Options
type AppError = [String]
data AppResult a = Either AppError a

newtype App a = App {
    runApp :: ReaderT Options (ExceptT AppError IO) a
} deriving (Monad, Functor, Applicative, AppConfig, MonadIO, MonadError AppError)

run :: App ()
run = liftIO . print "Success"
    =<< Render
    =<< Validate
    =<< Parser
    =<< readFile

readFile :: FilePath -> InputString
readFile file = case extension of
    ".json" -> JsonString $ Data.ByteString.Lazy.readFile file
    ".xml" -> XmlString $ readFile file
    _ -> return "Extension " ++ extension ++ " not supported." 
    where
        extension = toLower $ takeExtension file

parseFile :: InputString -> m PSettings
parseFile inputString = case parse inputString of
    Nothing -> 
    (Just pSettings) -> return pSettings

data AppError = Validation [String]
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