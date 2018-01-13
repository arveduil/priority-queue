import Test.QuickCheck
import qualified Data.PriorityQueue as Q


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

main :: IO ()
main = do
       putStrLn ""
       putStrLn "Verifying Leftist Property"
       quickCheck (verifyLeftist :: Q.Queue Int -> Bool)
       putStrLn "Verifying Heap Ordered Property"
       quickCheck (heapOrdered :: Q.Queue Int -> Bool)
       