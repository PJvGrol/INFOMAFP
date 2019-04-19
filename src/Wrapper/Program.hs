{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}

module Wrapper.Program (runProgram) where

-- Wrapper imports
import Wrapper.Options
import Wrapper.Parser.Data
import Wrapper.ChartData
import Wrapper.ErrorHandling

import qualified Wrapper.Parser.FileParser as WP
import qualified Wrapper.Validator.FileValidator as WV
import qualified Wrapper.Rendering.PlotRendering as WR

-- Other imports
import Data.Text
import Data.Default.Class
import Control.Monad.Reader
import Control.Monad.Except
import System.FilePath
import Graphics.Rendering.Chart.Renderable

import qualified Control.Exception as E
import qualified Data.Bifunctor as BF
import qualified Data.Char as C
import qualified Graphics.Rendering.Chart.Backend.Cairo as BEC
import qualified Data.ByteString.Lazy as By
import qualified Wrapper.Rendering.PlotRendering as R

-- Types
type AppConfig = MonadReader Options

newtype App a = App {
    runApp :: ReaderT Options (ExceptT AppError IO) a
} deriving (Monad, Functor, Applicative, AppConfig, MonadIO, MonadError AppError)

-- Program
runProgram :: Options -> IO ()
runProgram o = either renderError return =<< runExceptT (runReaderT (runApp run) o)

run :: App ()
run = do
    input <- readInput
    parsed <- handleParsing input
    validated <- handleValidation parsed
    result <- handleRendering validated
    liftIO $ putStrLn result

-- Helpers
handleRendering :: Settings Double Double -> App String
handleRendering settings = do 
    outputFile <- asks oFileToOutput
    renderedFile <- renderToFile outputFile settings
    liftIO $ renderedFile
    return "Successfully rendered your graph!"

handleValidation :: PSettings -> App (Settings Double Double)
handleValidation settings = either (throwError . ValidationError . toSList) return (WV.validate settings)

handleParsing :: InputString -> App PSettings
handleParsing input = case WP.parse input of
    (Just settings) -> return settings
    Nothing         -> throwError (ParseError "The input file could not be mapped to settings. Please verify you named all fields correctly.")

readInput :: App InputString
readInput = readFileFromOptions =<< asks oFileToRead
    where
        readFileFromOptions f = case unpack $ toLower $ takeExtension f of
            ".json" -> either throwError (return . JsonString) =<< (BF.first IOError <$> liftIO (safeReadJsonFile f))
            ".xml" -> either throwError (return . XmlString) =<< (BF.first IOError <$> liftIO (safeReadXmlFile f))
            o -> throwError (FileError $ "Unsupported file extension: " ++ o)

safeReadJsonFile :: FilePath -> IO (Either E.IOException By.ByteString)
safeReadJsonFile = E.try . By.readFile

safeReadXmlFile :: FilePath -> IO (Either E.IOException String)
safeReadXmlFile = E.try . readFile

renderToFile :: FilePath -> Settings Double Double -> App (IO (Graphics.Rendering.Chart.Renderable.PickFn ()))
renderToFile file settings = case unpack $ toLower $ takeExtension file of
    ".png" -> return $ inlineRender BEC.PNG
    ".svg" -> return $ inlineRender BEC.SVG
    ".ps" -> return $ inlineRender BEC.PS
    ".pdf" -> return $ inlineRender BEC.PDF
    o -> throwError $ RenderError ("Unsuppored file extension: " ++ o)
    where
        inlineRender t = let fileOptions = BEC.FileOptions (800,600) t in BEC.renderableToFile fileOptions file $ R.render settings

renderError :: AppError -> IO ()
renderError (IOError e) = do
    putStrLn "An error occurred when trying to load the input file:"
    putStrLn $ "  " ++ show e
renderError (FileError e) = do
    putStrLn "An error occurred when trying to load the input file:"
    putStrLn $ "  " ++ show e
renderError (ParseError e) = do
    putStrLn "An error occurred when trying to parse the input file:"
    putStrLn $ "  " ++ show e
renderError (ValidationError e) = do
    putStrLn "An error occurred when trying to validate the input file:"
    putStrLn $ "  " ++ show e
renderError (RenderError e) = do
    putStrLn "An error occurred when trying to render the input file:"
    putStrLn $ "  " ++ show e