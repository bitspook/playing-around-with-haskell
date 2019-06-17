module LogAnalysis
  ( parseMessage
  , parse
  , insert
  ) where

import           LogAnalysis.Internal

parseMessage :: String -> LogMessage
parseMessage str =
  case words str of
    "E":n:ts:ws -> LogMessage (Error $ read n) (read ts) (unwords ws)
    "I":ts:ws   -> LogMessage Info (read ts) (unwords ws)
    "W":ts:ws   -> LogMessage Warning (read ts) (unwords ws)
    otherwise   -> Unknown str

parse :: String -> [LogMessage]
parse = mapFunc parseMessage . lines
  where
    mapFunc func str =
      case str of
        []   -> []
        x:xs -> func x : mapFunc func xs

insert :: LogMessage -> MessageTree -> MessageTree
insert _ _ = Leaf
