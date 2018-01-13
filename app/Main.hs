module Main where

--import Test.QuickCheck
import Data.PriorityQueue as Q
    

main :: IO ()
main = do
   let e = emptyQ :: Queue Int
       q1 = push 10 e
       q2 = push 20 q1
       q3 = push 15 q2
       q4 = push 2 q3
   -- This should print 2 (minimum value)
   print (peek q4)
   -- This should remove minimum
   let q5 = Q.pop q4
   -- This should now print 10
   print (Q.peek q5)

