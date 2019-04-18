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

import Wrapper.Parser.Parser
import Wrapper.Parser.Data
import System.FilePath

import qualified Data.ByteString.Lazy as By

-- types

data Options = Options
    { 
        oFileToRead :: String
    }

type AppConfig = MonadReader Options

data AppError = IOError E.IOException | ParseError String


newtype App a = App {
    runApp :: ReaderT Options (ExceptT AppError IO) a
} deriving (Monad, Functor, Applicative, AppConfig, MonadIO, MonadError AppError)

-- program

runProgram :: Options -> IO ()
runProgram o = either renderError return =<< runExceptT (runReaderT (runApp run) o)

renderError :: AppError -> IO ()
renderError (IOError e) = do
    putStrLn "There was an error:"
    putStrLn $ "  " ++ show e
renderError (ParseError e) = do
    putStrLn "There was an error:"
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

loadContents :: App InputString
loadContents = readFileFromOptions =<< asks oFileToRead
    where
        readFileFromOptions f = case takeExtension f of
            "json" -> either throwError (return . JsonString) =<< (BF.first IOError <$> liftIO (safeReadJsonFile f))
            "xml" -> either throwError (return . XmlString) =<< (BF.first IOError <$> liftIO (safeReadXmlFile f))
            _ -> throwError (ParseError "test")

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

safeReadJsonFile :: FilePath -> IO (Either E.IOException By.ByteString)
safeReadJsonFile = E.try . By.readFile

safeReadXmlFile :: FilePath -> IO (Either E.IOException String)
safeReadXmlFile = E.try . readFile

-- safeReadFile :: FilePath -> Either E.IOException InputString
-- safeReadFile fp = case extension fp of
--                     ".json" -> BF.second JsonString <$> liftIO (safeReadJsonFile fp)           
--                     -- ".xml" -> BF.second XmlString <$> liftIO (safeReadXmlFile fp)
--                     -- _ -> userError "Input type not recognised"