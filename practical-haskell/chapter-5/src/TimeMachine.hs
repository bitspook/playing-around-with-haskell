module TimeMachine where

data TimeMachine = TM { manufacturer :: String, year :: Integer } deriving (Eq, Show)

timeMachinesFrom :: String -> Integer -> [TimeMachine]
timeMachinesFrom mf y = TM mf y : timeMachinesFrom mf (y+1)

timelyIncMachines :: [TimeMachine]
timelyIncMachines  = timeMachinesFrom "Timely Inc." 100
