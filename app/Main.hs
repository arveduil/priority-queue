module Main where

--import Test.QuickCheck
import Data.PriorityQueueBT as Q
    

main :: IO ()
main = do
   let e = emptyQ :: Queue Int
       q1 = push 1 e
       q2 = push 2 q1
       q3 = push 3 q2
       q4 = push 4 q3
   -- This should print 2 (minimum value)
   print (peek q4)
   -- This should remove minimum
   let q5 = Q.pop q4
   -- This should now print 10
   print (Q.peek q5)

