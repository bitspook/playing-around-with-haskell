module LogAnalysis
  ( parseMessage
  , parse
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
parse = fmap parseMessage . lines
