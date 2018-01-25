module PriorityQueueTestsL where
    import Test.QuickCheck
    import qualified Data.PriorityQueueBT as QBT
    import qualified Data.PriorityQueueL as QL
    import Data.List
    import Test.HUnit
        

    --testL = TestCase $ assertEqual "test push1 empty Queue" (QL.PQueue [1]) ( QL.pqPush 1 (QL.PQueue []) )
    --HUnit tests for push operation in list priority queue
    testPushL1 = TestCase $ assertEqual "test: push element to empty list queue" ( QL.PQueue[1] ) ( QL.pqPush 1 (PQueue[]) )
    testPushL2 = TestCase $ assertEqual "test: push greater  element to not empty list queue" ( QL.PQueue[1,2,3] ) ( QL.pqPush 3 (PQueue[1,2]) )
    testPushL3 = TestCase $ assertEqual "test: push mid      element to not empty list queue" ( QL.PQueue[1,2,3] ) ( QL.pqPush 2 (PQueue[1,3]) )
    testPushL4 = TestCase $ assertEqual "test: push smallest element to not empty list queue" ( QL.PQueue[1,2,3] ) ( QL.pqPush 1 (PQueue[2,3]) )

    --HUnit tests for pop operation in list priority queue
    testPopL1 = TestCase $ assertEqual "test: pop element from list queue" ( QL.PQueue[1] ) ( QL.pqPop (PQueue[2,1]) )

    --HUnit tests for peek operation in list priority queue
    testPeekL1 = TestCase $ assertEqual "test: peek operation on list queue" ( QL.3 ) ( QL.pqPeek (PQueue[3,2,1]) )

    --QuickCheck test, which tests whether list queue is in descending order or not
    checkOrder :: Ord a => QL.PQueue a -> Bool
    checkOrder QL.PQueue[] = True
    checkOrder QL.PQueue[x] = True
    checkOrder QL.PQueue(x:y:xs) =
        if(x>=y && checkOrder (y:ys) ) then True else False

    --QuickCheck test, which tests whether peek operation over list queue gives the greatest element from the queue or not
    checkGreatest :: Ord a => QL.PQueue a -> Bool
    checkGreatest QL.PQueue[x] = if( QL.pqPeek (PQueue[x]) == x ) then True else False
    checkGreatest QL.PQueue(x:xs) = if( x >= max(PQueue(xs)) ) then True else False

    --max returns the greatest value from list queue
    max :: Ord a => QL.PQueue a -> a
    max QL.PQueue[x] = x
    max QL.PQueue(x:xs) = if(x >= max QL.PQueue(xs)) then x else max QL.PQueue(xs)

    runTests :: IO ()
    runTests = do
        PutStrLn "HUnit tests for list priority queue"
        runTestTT testPushL1
        runTestTT testPushL2
        runTestTT testPushL3
        runTestTT testPushL4
        runTestTT testPopL1
        runTestTT testPeekL1

        PutStrLn " "
        quickCheck (checkOrder :: QL.Queue Int -> Bool)
        quickCheck (checkGreatest :: QL.Queue Int -> Bool)
