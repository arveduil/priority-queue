module Main where
import Data.PriorityQueueBT as QBT
import Data.PriorityQueueL as QL
import System.Environment (getArgs)
import System.Random 
import Data.Time.Clock (diffUTCTime, getCurrentTime)
--import Data.Time.Clock (diffUTCTime, getCurrentTime)
    

main :: IO ()
main = do
    g <- getStdGen
    print $ take 10 (randomRs ('a', 'z') g)

