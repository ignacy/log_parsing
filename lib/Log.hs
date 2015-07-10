module Log where

import Control.Applicative ( (<$>)     )
import Text.Read           ( readMaybe )

data RequestType = GET
                 | POST
                 | PUT
                 | DELETE
  deriving (Show, Eq)

type Path = String

data RequestLog = RequestLog RequestType Path
  deriving (Show, Eq)

data MaybeRequestLog = ValidRL RequestLog
                     | InvalidRL String
  deriving (Show, Eq)

parseRequestLog :: String -> MaybeRequestLog
parseRequestLog m = case lineHead of
  "Started" -> ValidRL (parseRequest lineRest)
  _         -> InvalidRL m
  where lineHead = head (words m)
        lineRest = drop 1 (words m)

parseRequest :: [String] -> RequestLog
parseRequest items = RequestLog requestType requestPath
  where requestType = (parseRequestType (head items))
        requestPath = head (drop 1 items)

parseRequestType :: String -> RequestType
parseRequestType t = case t of
  "GET"    -> GET
  "POST"   -> POST
  "PUT"    -> PUT
  "DELETE" -> DELETE

validRequestsOnly :: [MaybeRequestLog] -> [RequestLog]
validRequestsOnly [] = []
validRequestsOnly (x:xs) = case x of
                             ValidRL lm -> lm : validRequestsOnly xs
                             InvalidRL _ -> validRequestsOnly xs

parse :: String -> [RequestLog]
parse f = validRequestsOnly (map parseRequestLog (filter (\n -> n /= []) (lines f)))

-- | @testParse p n f@ tests the log file parser @p@ by running it
--   on the first @n@ lines of file @f@.
testParse :: (String -> [RequestLog])
          -> Int
          -> FilePath
          -> IO ()
testParse parse n file = do
  messages <- take n . parse <$> readFile file
  mapM_ (putStrLn . show) messages
