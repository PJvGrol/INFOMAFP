module ChartHelper where

import qualified Control.Exception

safeReadFile :: FilePath -> IO (Either E.IOException String)
safeReadFile = try . readFile