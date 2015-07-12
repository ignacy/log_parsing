module Log where

import Control.Applicative ( (<$>)     )
import Text.Read           ( readMaybe )
import Data.List

data RequestType = Get
                 | Post
                 | Put
                 | Delete
  deriving (Show, Eq)

type Path = String
type Date = String
data RequestLog = RequestLog RequestType Path Date
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
parseRequest items = RequestLog requestType requestPath requestDate
  where requestType = (parseRequestType (head items))
        requestPath = head (drop 1 items)
        requestDate = (concat (intersperse " " (drop 5 items)))

parseRequestType :: String -> RequestType
parseRequestType t = case t of
  "GET"    -> Get
  "POST"   -> Post
  "PUT"    -> Put
  "DELETE" -> Delete

validRequestsOnly :: [MaybeRequestLog] -> [RequestLog]
validRequestsOnly [] = []
validRequestsOnly (x:xs) = case x of
  ValidRL lm -> lm : validRequestsOnly xs
  InvalidRL _ -> validRequestsOnly xs


postRequestsOnly :: [RequestLog] -> [RequestLog]
postRequestsOnly [] = []
postRequestsOnly (x:xs) = case x of
                             (RequestLog Post _ _) -> x : postRequestsOnly xs
                             _ -> postRequestsOnly xs

parse :: String -> [RequestLog]
parse f = validRequestsOnly (map parseRequestLog (filter (\n -> (length n) > 3) (lines f)))

findPOSTRequests :: String -> [RequestLog]
findPOSTRequests f = postRequestsOnly (parse f)

testParse :: (String -> [RequestLog])
          -> Int
          -> FilePath
          -> IO ()
testParse parse n file = do
  messages <- take n . parse <$> readFile file
  mapM_ (putStrLn . show) messages
