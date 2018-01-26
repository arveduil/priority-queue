-- |module for Priority Queue base on a Binary Tree
module Data.PriorityQueueBT where

 data Queue a = Empty | Queue Int a (Queue a) (Queue a)          deriving Show

 -- |Eq needed for HUnit tests
 instance Eq a  => Eq (Queue a)where
  (==) (Empty) (Empty) = True
  (==) (Queue _ _ _ _) (Empty) = False
  (==) (Empty) (Queue _ _ _ _) = False
  (==) (Queue rank1 v1 l1 r1)  (Queue rank2 v2 l2 r2) = rank1 == rank2 && v1 == v2 && l1 == l2 && r1 == r2

 -- |mergeQs is merging queues with rules of heap 
 mergeQs :: Ord a => Queue a -> Queue a -> Queue a
 mergeQs Empty q = q
 mergeQs q Empty = q
 mergeQs left@(Queue _ lv ll lr) right@(Queue _ rv rl rr) =
                  if lv >= rv then
                   swipe lv ll (mergeQs lr right)
                 else
                   swipe rv rl (mergeQs left rr)


 -- |rank get value of rank for each Queue
 rank :: Queue a -> Int
 rank Empty = 0
 rank (Queue r _ _ _) = r

 -- |swipe add values and join left and right
 swipe :: a -> Queue a -> Queue a -> Queue a
 swipe v left right =
    if rank left >= rank right then
      Queue (rank right + 1) v left right
    else
     Queue (rank left + 1) v right left
 
 -- |returns Empty
 emptyQ :: Queue a
 emptyQ = Empty

 -- |singletonQ returns Queue with single value 
 singletonQ :: Ord a =>  a -> Queue a
 singletonQ v = Queue 1 v Empty Empty
 
 -- |push value  to the queue
 push :: Ord a => a -> Queue a -> Queue a
 push v q = mergeQs (singletonQ v) q

 -- |read value from head without removing
 peek :: Queue a ->  a
 peek Empty = error "Nothing inside!"
 peek (Queue _ v _ _) =  v

 -- |remove value from head
 pop :: Ord a => Queue a -> Queue a
 pop Empty = error "Nothing inside!"
 pop (Queue _ v l r) =  mergeQs l r
 
 -- |check if the queue is empty
 isEmpty :: Queue a -> Bool
 isEmpty Empty = True
 isEmpty (Queue _ _ _ _) = False
