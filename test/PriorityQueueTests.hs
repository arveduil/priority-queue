import Test.QuickCheck
import qualified Data.PriorityQueueBT as QBT
import qualified Data.PriorityQueueL as QL
import Data.List
import Test.HUnit


instance (Arbitrary a, Ord a) => Arbitrary (QBT.Queue a) where
    arbitrary = listOf arbitrary >>= genQueueFromList

genQueueFromList :: Ord a => [a] -> Gen (QBT.Queue a)
genQueueFromList xs = return (foldr QBT.push QBT.emptyQ xs)

qFromList :: Ord a => [a] -> QBT.Queue a
qFromList xs = foldr QBT.push QBT.emptyQ xs

listFromQ :: Ord a => QBT.Queue a -> [a]
listFromQ QBT.Empty = []
listFromQ q@(QBT.Queue rank v l r) = [QBT.peek q] ++ listFromQ (QBT.pop q) 

qrank :: QBT.Queue a -> Int
qrank QBT.Empty = 0
qrank (QBT.Queue _ _ l r) = 1 + minimum [qrank l, qrank r]

--QuickCheck test, which test poping the greatest 
verifyGreatest :: Ord a => QBT.Queue a -> Bool
verifyGreatest QBT.Empty = True
verifyGreatest q@(QBT.Queue rnk v l r) =  if (QBT.isEmpty (QBT.pop q)) then True
                                                             else if ( QBT.isEmpty (QBT.pop (QBT.pop q)) ) then True
                                                             else QBT.peek q >= QBT.peek (QBT.pop q) && (verifyGreatest (QBT.pop q))

--QuickCheck test, which test whether the binary tree is balanced
verifyLeftist :: QBT.Queue a -> Bool
verifyLeftist QBT.Empty = True
verifyLeftist q@(QBT.Queue rnk v l r) =
       and [ qrank q == rnk
       , qrank l >= qrank r
       , verifyLeftist l
       , verifyLeftist r ]

--QuickCheck test, which test whether the binary tree is heapOrdered
heapOrdered :: Ord a => QBT.Queue a -> Bool
heapOrdered QBT.Empty = True
heapOrdered (QBT.Queue _ _ QBT.Empty QBT.Empty) = True
heapOrdered (QBT.Queue _ v QBT.Empty r@(QBT.Queue _ rv _ _)) =
                     and [ v >= rv, heapOrdered r ]
heapOrdered (QBT.Queue _ v l@(QBT.Queue _ lv _ _) QBT.Empty) =
                     and [ v >= lv, heapOrdered l ]
heapOrdered (QBT.Queue _ v l@(QBT.Queue _ lv _ _) r@(QBT.Queue _ rv _ _)) =
                   and [ v >= lv, v >= rv, heapOrdered l, heapOrdered r]

--HUnit tests for push operation
testPush1 = TestCase $ assertEqual "test push1 empty Queue" (QBT.Queue 1 1 QBT.Empty QBT.Empty) (QBT.push 1 (QBT.emptyQ))
testPush2 = TestCase $ assertEqual "test push2 with values inc" (QBT.Queue 1 4 (QBT.Queue 1 3 (QBT.Queue 1 2 (QBT.Queue 1 1 QBT.Empty QBT.Empty) QBT.Empty) QBT.Empty) QBT.Empty) ( QBT.push 4 $ QBT.push 3 $ QBT.push 2 $ QBT.push 1 $ QBT.emptyQ)
testPush3 = TestCase $ assertEqual "test push3 with values dec" (QBT.Queue 2 4 (QBT.Queue 1 3 QBT.Empty QBT.Empty) (QBT.Queue 1 2 (QBT.Queue 1 1 QBT.Empty QBT.Empty) QBT.Empty)) ( QBT.push 1 $ QBT.push 2 $ QBT.push 3 $ QBT.push 4 $ QBT.emptyQ)
testPush4 = TestCase $ assertEqual "test push4 wih different values" ( reverse $ sort [0,213,435,46,657,87,324,6675 , 6576]) (listFromQ $ qFromList [0,213,435,46,657,87,324,6675 , 6576])

--HUnit tests for peek operation
testPeek1= TestCase $ assertEqual "test peek1  Queue" 1 (QBT.peek (QBT.push 1 (QBT.emptyQ)))
testPeek2= TestCase $ assertEqual "test peek2  Queue" (maximum [12 ,321, 435, 657, 657, 123]) (QBT.peek (qFromList [12, 321, 435, 657, 657, 123]))

--HUnit tests for pop and peek operation
testPopPeek1= TestCase $ assertEqual "test popPeek  Queue" 3 (  QBT.peek $ QBT.pop $ QBT.push 4 $ QBT.push 3 $ QBT.push 2 $ QBT.push 1 $ QBT.emptyQ)
--adding tests to TestLists
testListPush = TestList [testPush1, testPush2, testPush3, testPush4]
testListPeek = TestList [testPeek1,testPeek2]
testListPopPeek = TestList [testPopPeek1]
--koniec testow do PQBT
testL = TestCase $ assertEqual "test push1 empty Queue" (QL.PQueue [1]) ( QL.pqPush 1 (QL.PQueue []) )


main :: IO ()
main = do
       putStrLn "HUnit tests for push operation for BT" 
       runTestTT testListPush
       putStrLn "HUnit tests for peek operation for BT"      
       runTestTT testListPeek
       putStrLn "HUnit tests for pop and peek operation for BT"      
       runTestTT testListPopPeek       
       putStrLn "HUnit tests for List"
       runTestTT testL 

       putStrLn ""
       putStrLn "Verifying Leftist Property"
       quickCheck (verifyLeftist :: QBT.Queue Int -> Bool)
       putStrLn "Verifying Heap Ordered Property"
       quickCheck (heapOrdered :: QBT.Queue Int -> Bool)
       putStrLn "Verifying Greater Ordered Property"
       quickCheck (verifyGreatest :: QBT.Queue Int -> Bool)
       
       
       

