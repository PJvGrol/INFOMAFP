{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}

module Wrapper.Program where

import qualified Control.Exception as E
import           Control.Monad.Reader
import           Control.Monad.Except
import qualified Data.Bifunctor as BF
import qualified Data.Bool as B
import qualified Data.Char as C
import           Options.Applicative
import Data.Semigroup ((<>))

import qualified Wrapper.Parser.Parser as WP
import Wrapper.Parser.Data
import qualified Wrapper.Validator.Validator as WV
import System.FilePath

import qualified Data.ByteString.Lazy as By

-- types

data Options = Options
    { 
        oFileToRead :: String
    }

type AppConfig = MonadReader Options

data AppError = IOError E.IOException | FileError String | ParseError String | ValidationError String | RenderError String


newtype App a = App {
    runApp :: ReaderT Options (ExceptT AppError IO) a
} deriving (Monad, Functor, Applicative, AppConfig, MonadIO, MonadError AppError)

-- program

runProgram :: Options -> IO ()
runProgram o = either renderError return =<< runExceptT (runReaderT (runApp run) o)

renderError :: AppError -> IO ()
renderError (IOError e) = do
    putStrLn "An error occurred when trying to load the input file:"
    putStrLn $ "  " ++ show e
renderError (FileError e) = do
    putStrLn "An error occurred when trying to load the input file:"
    putStrLn $ "  " ++ show e
renderError (ParseError e) = do
    putStrLn "An error occurred when trying to load the input file:"
    putStrLn $ "  " ++ show e
renderError (ValidationError e) = do
    putStrLn "An error occurred when trying to validate the input file:"
    putStrLn $ "  " ++ show e
renderError (RenderError e) = do
    putStrLn "An error occurred when trying to render the input file:"
    putStrLn $ "  " ++ show e

run :: App ()
run = liftIO . print =<< loadContents

-- data retrieval and transformation

-- getSource :: App String
-- getSource = B.bool loadContents (liftIO getContents) =<< asks oStdIn

-- handleRendering :: AppConfig m => String -> m String
-- handleRendering s = B.bool s (map C.toUpper s) <$> asks oCapitalize

-- handleValidation :: PSettings -> App Settings
-- handleValidation s = B.bool s ("ZOMG " ++ s) <$> asks oExcited

-- handleParsing :: InputString -> App PSettings
-- handleParsing inputString = case parse inputString of
--     Nothing -> throwError "An error occured during parsing"
--     (Just pSettings) -> return pSettings
handleRendering :: Settings Double Double -> App ()
handleRendering settings = undefined

handleValidation :: PSettings -> App (Settings Double Double)
handleValidation settings = either (throwError . ValidationError) return (WV.parse settings)

handleParsing :: InputString -> App PSettings
handleParsing input = case WP.parse input of
    (Just settings) -> return settings
    Nothing         -> throwError (ParseError "The input file could not be mapped to settings. Please verify you named all fields correctly.")

--- Load the input file ---

loadContents :: App InputString
loadContents = readFileFromOptions =<< asks oFileToRead
    where
        readFileFromOptions f = case takeExtension f of
            "json" -> either throwError (return . JsonString) =<< (BF.first IOError <$> liftIO (safeReadJsonFile f))
            "xml" -> either throwError (return . XmlString) =<< (BF.first IOError <$> liftIO (safeReadXmlFile f))
            o -> throwError (FileError $ "Unknown file extension: " ++ o)

safeReadJsonFile :: FilePath -> IO (Either E.IOException By.ByteString)
safeReadJsonFile = E.try . By.readFile

safeReadXmlFile :: FilePath -> IO (Either E.IOException String)
safeReadXmlFile = E.try . readFile

--- Handle command line options ---

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