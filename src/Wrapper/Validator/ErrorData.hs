module ErrorData where

data ErrorList = Errors [ValError]

data ValError = RequieredFieldMissing String
                | DataNotMatchesType String
                | UnknownGraphType String
                | UnknownOutPutType String