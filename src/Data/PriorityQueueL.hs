module Data.PriorityQueueL where

 data PQueue a = PQueue [a] deriving (Show)

 --pop for priority queue
 pqPop :: PQueue a -> PQueue a
 pqPop (PQueue []) = (PQueue []) --error "Nothing inside!"
 pqPop (PQueue (x:xs)) = PQueue (xs)

--peak for priority queue
 pqPeek :: PQueue a -> a
 pqPeek (PQueue []) = error "Nothing inside!"
 pqPeek (PQueue (x:xs)) = x


--push for priority queue
 pqPush :: Ord a => a -> PQueue a -> PQueue a
 pqPush  a (PQueue [])  = (PQueue [a])
 pqPush  a (PQueue l) = (PQueue (insertToList a l))
        where
            insertToList a [] = [a]
            insertToList a (x:xs) = if( a > x) then (a:x:xs) else (x : (insertToList a xs))     
            


   
