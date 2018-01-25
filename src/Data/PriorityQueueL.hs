--pop for priority queue
pqPop :: [a] -> [a]
pqPop [] = error "Nothing inside!"
pqPop (x:xs) = xs

--peek for priority queue
pqPeek :: [a] -> a
pqPeek [] = error "Nothing inside!"
pqPeek (x:xs) = x

--push for priority queue
pqPush :: Ord a => (a, [a]) -> [a]

pqPush ( a, [] ) = [a]
pqPush ( a, (x:xs) ) =
    if( a > x)
        then (a:x:xs)
    else
        [x] ++ pqPush( a, (xs) )
