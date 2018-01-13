import Test.QuickCheck
import qualified Data.PriorityQueue as Q

import Test.HUnit
--import Test.Framework
--import Test.Framework.Providers.HUnit
--import Data.Monoid
--import Control.Monad



instance (Arbitrary a, Ord a) => Arbitrary (Q.Queue a) where
    arbitrary = listOf arbitrary >>= qFromList




qFromList :: Ord a => [a] -> Gen (Q.Queue a)
qFromList xs = return (foldr Q.push Q.emptyQ xs)



qrank :: Q.Queue a -> Int
qrank Q.Empty = 0
qrank (Q.Queue _ _ l r) = 1 + minimum [qrank l, qrank r]

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
testPush3 = TestCase $ assertEqual "test push2 with values dec" (Q.Queue 1 4 (Q.Queue 1 3 (Q.Queue 1 2 (Q.Queue 1 1 Q.Empty Q.Empty) Q.Empty) Q.Empty) Q.Empty) ( Q.push 1 $ Q.push 2 $ Q.push 3 $ Q.push 4 $ Q.emptyQ)

testList = TestList [testPush1, testPush2, testPush3]

--    "Push to empty" ~: do let e = Q.emptyQ :: Q.Queue Int
--                                                q1 = Q.push 1 e 
--                                                assertEqual "Push to empty" (Q.Queue 1 1 Q.Empty Q.Empty) ( q1)                    
--                    ]
                                                --                     , "Push to queue" ~: do let e = Q.emptyQ :: Q.Queue Int
--                                                               q1 = Q.push 4 e
--                                                               q2 = Q.push 3 q1
--                                                               q3 = Q.push 2 q2
--                                                              q4 = Q.push 1 q3 
--                                                               assertEqual "Push to queue with values" (Q.Queue 1 4 (Q.Queue 1 3 (Q.Queue 1 2 (Q.Queue 1 1 Q.Empty Q.Empty) Q.Empty) Q.Empty) Q.Empty) ( q4)                                                         
--                     , "Push to queue" ~: do     let e = Q.emptyQ :: Q.Queue Int
--                                                                    q1 = Q.push 1 e
--                                                                    q2 = Q.push 2 q1
--                                                                   q3 = Q.push 3 q2
--                                                                    q4 = Q.push 4 q3 
--                                                                   assertEqual "Push to queue with values Inc" (Q.Queue 1 4 (Q.Queue 1 3 (Q.Queue 1 2 (Q.Queue 1 1 Q.Empty Q.Empty) Q.Empty) Q.Empty) Q.Empty) ( q4) 
--                    ]  

--testPeak = TestList [
--                    
--                     "Peak from basic queue" ~: (do let e = Q.emptyQ :: Q.Queue Int
--                                                        q1 = Q.push 1 e
--                                                        q2 = Q.push 2 q1
--                                                        q3 = Q.push 3 q2
--                                                        q4 = Q.push 4 q3 
--                                                        assertEqual "Peak to queue with values" (4) ( peak q4) )                    
--                    ]


main :: IO ()
main = do
       runTestTT testList
       putStrLn ""
       putStrLn "Verifying Leftist Property"
       quickCheck (verifyLeftist :: Q.Queue Int -> Bool)
       putStrLn "Verifying Heap Ordered Property"
       quickCheck (heapOrdered :: Q.Queue Int -> Bool)
       
       

