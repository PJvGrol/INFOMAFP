module Wrapper.ErrorHandling where

import qualified Control.Exception as E

-- Types
data AppError = IOError E.IOException 
    | FileError String 
    | ParseError String 
    | ValidationError String 
    | RenderError String

newtype ErrorList = Errors [ValError]

data ValError = RequiredFieldMissing String
                | DataNotMatchesType String
                | UnknownGraphType String
                | UnknownOutPutType String
                | NoErrorsFound String

-- Instances
instance Show ValError where
    show = toString

instance Show ErrorList where
    show = toSList

-- Helpers
{- Simple function that combines to ErrorLists. -}                 
combine :: ErrorList -> ErrorList -> ErrorList
combine (Errors e1) (Errors e2) = Errors (e1 ++ e2) 

{- Simple function that combines all errors into one string. -}
toSList :: ErrorList -> String
toSList (Errors ls) = foldr ((++) . (++ "\n") . show) "" ls 

toString :: ValError -> String
toString (RequiredFieldMissing s) = s
toString (DataNotMatchesType s)    = s
toString (UnknownGraphType s)      = s
toString (UnknownOutPutType s)     = s
toString (NoErrorsFound s)         = s
