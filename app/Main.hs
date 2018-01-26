module Main where
import Data.PriorityQueueBT as QBT
import Data.PriorityQueueL as QL
import System.Environment (getArgs)
import System.Random 
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import Control.Parallel (par, pseq)
--import Data.Time.Clock (diffUTCTime, getCurrentTime)
    
qBTFromList :: Ord a => [a] -> QBT.Queue a
qBTFromList xs = foldr QBT.push QBT.emptyQ xs

qLFromList :: Ord a => [a] -> QL.PQueue a
qLFromList xs = foldr QL.pqPush (QL.PQueue []) xs

qBTPopAll :: Ord a => QBT.Queue a ->QBT.Queue a
qBTPopAll QBT.Empty = QBT.Empty 
qBTPopAll q@ (Queue _ _ _ _) = qBTPopAll $ QBT.pop q

qLPopAll :: Ord a => QL.PQueue a -> QL.PQueue a 
qLPopAll (QL.PQueue []) = (QL.PQueue [])
qLPopAll (QL.PQueue (x:xs)) = qLPopAll $ QL.pqPop (QL.PQueue (x:xs))

randomList :: Int -> IO([Int])
randomList 0 = return []
randomList n = do
  r  <- randomRIO (1,6)
  rs <- randomList (n-1)
  return (r:rs) 

force :: [a] -> ()
force xs = go xs `pseq` ()
    where go (_:xs) = go xs
          go [] = 1

-- | Function to generate random list of Ints.
randomInts :: Int -> StdGen -> [Int]
randomInts k g = 
    let result = take k (randoms g)
    in force result `seq` result

main :: IO ()
main = do
    let count = 8000000
    input <- randomInts count `fmap` getStdGen
    start1 <- getCurrentTime
    putStrLn $ ""
    let q1 = qBTFromList input
    let q2 = qBTPopAll q1
    end1 <- getCurrentTime
    putStrLn $  show (end1 `diffUTCTime` start1) ++ " for "++(show count) ++ " data pushed and then poped from PQ on BinaryTree\n"
    start1 <- getCurrentTime
    putStrLn $ ""
    let   q1 = qLFromList input
    let q2 = qLPopAll q1
    end1 <- getCurrentTime
    putStrLn $ show (end1 `diffUTCTime` start1) ++ " for "++(show count) ++ " data pushed and then poped from PQ on list \n"

