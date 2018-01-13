module Data.PriorityQueue where

 data Queue a = Empty | Queue Int a (Queue a) (Queue a)          deriving Show


 mergeQs :: Ord a => Queue a -> Queue a -> Queue a
 mergeQs Empty q = q
 mergeQs q Empty = q
 mergeQs left@(Queue _ lv ll lr) right@(Queue _ rv rl rr) =
                  if lv >= rv then
                   swipe lv ll (mergeQs lr right)
                 else
                   swipe rv rl (mergeQs left rr)

 rank :: Queue a -> Int
 rank Empty = 0
 rank (Queue r _ _ _) = r

 swipe :: a -> Queue a -> Queue a -> Queue a
 swipe v left right =
    if rank left >= rank right then
      Queue (rank right + 1) v left right
    else
     Queue (rank left + 1) v right left

 emptyQ :: Queue a
 emptyQ = Empty

 singletonQ :: a -> Queue a
 singletonQ v = Queue 1 v Empty Empty

 push :: Ord a => a -> Queue a -> Queue a
 push v q = mergeQs (singletonQ v) q

 peek :: Queue a ->  a
 peek Empty = error "Nothing inside!"
 peek (Queue _ v _ _) =  v

 pop :: Ord a => Queue a -> Queue a
 pop Empty = error "Nothing inside!"
 pop (Queue _ v l r) =  mergeQs l r

 isEmpty :: Queue a -> Bool
 isEmpty Empty = True
 isEmpty (Queue _ _ _ _) = False

-- showPoped :: (Maybe a, Queue a) -> a
 --showPoped (Nothing , _) = error "Nothing inside!"
--showPoped (Just a, _) = a

 --extractQueue :: (Maybe a, Queue a) -> Queue a
-- extractQueue (_, Queue ra v l r) = (Queue  ra v l r )

 --extractMaybe :: Maybe a -> a
 --extractMaybe Nothing = error "Nothing inside!"
 --extractMaybe (Just x) = x