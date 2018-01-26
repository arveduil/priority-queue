-- | 
module Main where
import Data.PriorityQueueBT as QBT
import Data.PriorityQueueL as QL
import System.Environment (getArgs)
import System.Random 
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import Control.Parallel (par, pseq)
<<<<<<< Updated upstream
--import Data.Time.Clock (diffUTCTime, getCurrentTime)
=======
>>>>>>> Stashed changes
    
qBTFromList :: Ord a => [a] -> QBT.Queue a
qBTFromList xs = foldr QBT.push QBT.emptyQ xs

<<<<<<< Updated upstream
=======
qBTPushPushPopList :: Ord a => [a] -> QBT.Queue a -> QBT.Queue a
qBTPushPushPopList (x:y:zs) q@(QBT.Empty)=  qBTPushPushPopList zs (QBT.pop (QBT.push y (QBT.push x q))) 
qBTPushPushPopList [x] q@(QBT.Empty)=  (QBT.pop  (QBT.push x q)) 
qBTPushPushPopList [] q@(QBT.Empty)=  q
qBTPushPushPopList (x:y:zs) q@(QBT.Queue _ _ _ _)=  qBTPushPushPopList zs (QBT.pop (QBT.push y (QBT.push x q))) 
qBTPushPushPopList [x] q@(QBT.Queue _ _ _ _)=  (QBT.pop  (QBT.push x q)) 
qBTPushPushPopList []  q@(QBT.Queue _ _ _ _)=  q

qLPushPushPopList :: Ord a => [a] -> QL.PQueue a ->  QL.PQueue  a
qLPushPushPopList (x:y:zs) q@(QL.PQueue a)=  qLPushPushPopList zs (pqPop (pqPush y (pqPush x q))) 
qLPushPushPopList [x]  q@(QL.PQueue a)=  (QL.pqPop  (QL.pqPush x q)) 
qLPushPushPopList []  q@(QL.PQueue a)=  q

>>>>>>> Stashed changes
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

<<<<<<< Updated upstream
-- | Function to generate random list of Ints.
=======
>>>>>>> Stashed changes
randomInts :: Int -> StdGen -> [Int]
randomInts k g = 
    let result = take k (randoms g)
    in force result `seq` result

main :: IO ()
main = do
<<<<<<< Updated upstream
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
=======
    putStrLn $ "==============================================="
    let count = 10000
    input <- randomInts count `fmap` getStdGen
    start1 <- getCurrentTime
    putStrLn $ "Star pushing and poping"
    let !q1 = qBTFromList input
    let !q2 = qBTPopAll q1
    putStrLn $ "End pushing and poping"
    end1 <- getCurrentTime
    putStrLn $  show (end1 `diffUTCTime` start1) ++ " for "++(show count) ++ " data pushed and then poped from PQ on BinaryTree\n"
    start1 <- getCurrentTime
    putStrLn $ "Star pushing and poping"
    let !q1 = qLFromList input
    let !q2 = qLPopAll q1
    putStrLn $ "End pushing and poping"
    end1 <- getCurrentTime
    putStrLn $ show (end1 `diffUTCTime` start1) ++ " for "++(show count) ++ " data pushed and then poped from PQ on list \n"
    start1 <- getCurrentTime
    putStrLn $ "Star push, push and pop"
    let   !q1 = qBTPushPushPopList  input QBT.Empty
    let   !q2 = qBTPushPushPopList  input q1
    putStrLn $ "End push, push and pop"
    end1 <- getCurrentTime
    putStrLn $ show (end1 `diffUTCTime` start1) ++ " for "++(show count) ++ " data pushed, pushed, and then poped from PQ on BinaryTree \n"
    start1 <- getCurrentTime
    putStrLn $ "Star push, push and pop"
    let   !q1 = qLPushPushPopList  input ( QL.PQueue [])
    let   !q2 = qLPushPushPopList  input q1
    putStrLn $ "End push, push and pop"
    end1 <- getCurrentTime
    putStrLn $ show (end1 `diffUTCTime` start1) ++ " for "++(show count) ++ " data pushed, pushed, and then poped from PQ on List \n"
>>>>>>> Stashed changes

