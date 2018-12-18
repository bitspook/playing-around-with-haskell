module LogAnalysis
  ( parseMessage
  ) where

import           LogAnalysis.Internal

parseMessage :: String -> LogMessage
parseMessage str =
  case words str of
    "E":n:ts:ws -> LogMessage (Error $ read n) (read ts) (unwords ws)
    "I":ts:ws   -> LogMessage Info (read ts) (unwords ws)
    otherwise   -> Unknown str
