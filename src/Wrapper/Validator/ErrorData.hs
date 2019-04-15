{- Module that contains the DataType for Error messages and some useful functions
   for them -}
module Wrapper.Validator.ErrorData where

data ErrorList = Errors [ValError] deriving (Show, Eq)

data ValError = RequiredFieldMissing String
                | DataNotMatchesType String
                | UnknownGraphType String
                | UnknownOutPutType String
                | NoErrorsFound String
                 deriving (Show, Eq)
 
{- Simple function that combines to ErrorLists. -}                 
combine :: ErrorList -> ErrorList -> ErrorList
combine (Errors e1) (Errors e2) = Errors (e1 ++ e2) 

{- Simple function that combines all errors into one string. -}
toSList :: ErrorList -> String
toSList (Errors ls) = foldr ((++).(\x -> x ++ "\n").toString) ("") ls 

toString :: ValError -> String
toString (RequiredFieldMissing s) = s
toString (DataNotMatchesType s)    = s
toString (UnknownGraphType s)      = s
toString (UnknownOutPutType s)     = s
toString (NoErrorsFound s)         = s