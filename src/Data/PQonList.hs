data PQueue a = PQueue[a] deriving (Show)

--pop for priority queue
pqPop :: PQueue a -> PQueue a
pqPop PQueue[] = PQueue[] --error "Nothing inside!"
pqPop PQueue (x:xs) = PQueue (xs)

--peek for priority queue
pqPeek :: PQueue a -> a
pqPeek PQueue[] = error "Nothing inside!"
pqPeek PQueue (x:xs) = x

--push for priority queue
pqPush :: Ord a => (a, PQueue a) -> PQueue a

pqPush ( a, PQueue[] ) = PQueue[a]
pqPush ( a, PQueue(x:xs) ) =
    if( a > x)
        then PQueue(a:x:xs)
    else
        PQueue [x] ++ pqPush( a, (xs) )
