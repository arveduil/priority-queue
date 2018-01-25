import Test.QuickCheck
import qualified Data.PriorityQueueBT as Q
import Data.List
import Test.HUnit


instance (Arbitrary a, Ord a) => Arbitrary (Q.Queue a) where
    arbitrary = listOf arbitrary >>= genQueueFromList



genQueueFromList :: Ord a => [a] -> Gen (Q.Queue a)
genQueueFromList xs = return (foldr Q.push Q.emptyQ xs)

qFromList :: Ord a => [a] -> Q.Queue a
qFromList xs = foldr Q.push Q.emptyQ xs

listFromQ :: Ord a => Q.Queue a -> [a]
listFromQ Q.Empty = []
listFromQ q@(Q.Queue rank v l r) = [Q.peak q] ++ listFromQ (Q.pop q) 

qrank :: Q.Queue a -> Int
qrank Q.Empty = 0
qrank (Q.Queue _ _ l r) = 1 + minimum [qrank l, qrank r]

verifyGreatest :: Ord a => Q.Queue a -> Bool
verifyGreatest Q.Empty = True
verifyGreatest q@(Q.Queue rnk v l r) =  if Q.pop q == Q.Empty then True
                                                             else if Q.pop (Q.pop q) == Q.Empty then True
                                                             else Q.peak q >= Q.peak (Q.pop q) && (verifyGreatest (Q.pop q))

verifyLeftist :: Q.Queue a -> Bool
verifyLeftist Q.Empty = True
verifyLeftist q@(Q.Queue rnk v l r) =
       and [ qrank q == rnk
       , qrank l >= qrank r
       , verifyLeftist l
       , verifyLeftist r ]


heapOrdered :: Ord a => Q.Queue a -> Bool
heapOrdered Q.Empty = True
heapOrdered (Q.Queue _ _ Q.Empty Q.Empty) = True
heapOrdered (Q.Queue _ v Q.Empty r@(Q.Queue _ rv _ _)) =
                     and [ v >= rv, heapOrdered r ]
heapOrdered (Q.Queue _ v l@(Q.Queue _ lv _ _) Q.Empty) =
                     and [ v >= lv, heapOrdered l ]
heapOrdered (Q.Queue _ v l@(Q.Queue _ lv _ _) r@(Q.Queue _ rv _ _)) =
                   and [ v >= lv, v >= rv, heapOrdered l, heapOrdered r]


testPush1 = TestCase $ assertEqual "test push1 empty Queue" (Q.Queue 1 1 Q.Empty Q.Empty) (Q.push 1 (Q.emptyQ))
testPush2 = TestCase $ assertEqual "test push2 with values inc" (Q.Queue 1 4 (Q.Queue 1 3 (Q.Queue 1 2 (Q.Queue 1 1 Q.Empty Q.Empty) Q.Empty) Q.Empty) Q.Empty) ( Q.push 4 $ Q.push 3 $ Q.push 2 $ Q.push 1 $ Q.emptyQ)
testPush3 = TestCase $ assertEqual "test push3 with values dec" (Q.Queue 2 4 (Q.Queue 1 3 Q.Empty Q.Empty) (Q.Queue 1 2 (Q.Queue 1 1 Q.Empty Q.Empty) Q.Empty)) ( Q.push 1 $ Q.push 2 $ Q.push 3 $ Q.push 4 $ Q.emptyQ)
testPush4 = TestCase $ assertEqual "test push4 wih different values" ( reverse $ sort [0,213,435,46,657,87,324,6675 , 6576]) (listFromQ $ qFromList [0,213,435,46,657,87,324,6675 , 6576])

testPeak1= TestCase $ assertEqual "test peak1  Queue" 1 (Q.peak (Q.push 1 (Q.emptyQ)))
testPeak2= TestCase $ assertEqual "test peak2  Queue" (maximum [12 ,321, 435, 657, 657, 123]) (Q.peak (qFromList [12, 321, 435, 657, 657, 123]))

testPopPeak1= TestCase $ assertEqual "test popPeak  Queue" 3 (  Q.peak $ Q.pop $ Q.push 4 $ Q.push 3 $ Q.push 2 $ Q.push 1 $ Q.emptyQ)



testListPush = TestList [testPush1, testPush2, testPush3, testPush4]
testListPeak = TestList [testPeak1,testPeak2]
testListPopPeak = TestList [testPopPeak1]




main :: IO ()
main = do
       putStrLn ""
       runTestTT testListPush
       runTestTT testListPeak      
       runTestTT testListPopPeak       
       
       putStrLn ""
       putStrLn "Verifying Leftist Property"
       quickCheck (verifyLeftist :: Q.Queue Int -> Bool)
       putStrLn "Verifying Heap Ordered Property"
       quickCheck (heapOrdered :: Q.Queue Int -> Bool)
       putStrLn "Verifying Greater Ordered Property"
       quickCheck (verifyGreatest :: Q.Queue Int -> Bool)
       
       

