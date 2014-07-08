{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

toInt :: String -> Int
toInt str = read str :: Int

parseMessage :: String -> LogMessage
parseMessage msg = case words msg of
  ("I" : t : str) -> LogMessage Info (toInt t) (unwords str)
  ("W" : t : str) -> LogMessage Warning (toInt t) (unwords str)
  ("E" : code : t : str) -> LogMessage (Error (toInt code)) (toInt t) (unwords str)
  _ -> Unknown msg

parse :: String -> [LogMessage]
parse = map parseMessage
      . lines

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert msg Leaf = (Node Leaf msg Leaf)
insert new@(LogMessage _ time _ ) (Node l m@(LogMessage _ t _) r)
  | time < t = (Node (insert new l) m r)
  | time >= t =  (Node l m (insert new r))

build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node l m r) = inOrder l ++ [m] ++ inOrder r

message :: LogMessage -> String
message (LogMessage _ _ s) = s

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = map message
              . filter isSevere
              . inOrder
              . build

isSevere :: LogMessage -> Bool
isSevere (LogMessage (Error sev) _ _) = (sev >= 50)
isSevere _ = True
