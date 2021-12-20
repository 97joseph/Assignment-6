module Final2
  (Map, isKeyM, emptyM, lookupM, mkM, updateM, updatesM, mapValueM,
   V, G, nodes, mkGraph, printGraph, filterG, foldrG)
where

import Data.List

-- Maps --

-- In any map there are no pairs (x,y) and (x',y') with x==x'.
type Map a b = [(a,b)]

isKeyM:: Eq a => a -> Map a b -> Bool
isKeyM k = any ((==) k . fst)
                
emptyM :: Map a b
emptyM = []

lookupM :: Eq a => a -> Map a b -> Maybe b
lookupM = lookup

mkM :: [(a,b)] -> Map a b
mkM = id

updateM :: Eq a => a -> b -> Map a b -> Map a b
updateM x y [] = [(x,y)]
updateM x y ((x',y'):l) | x==x' = (x,y):l
updateM x y (p:l) = p : updateM x y l

updatesM :: Eq a => [a] -> [b] -> Map a b -> Map a b
updatesM xs ys m = foldr (uncurry updateM) m (zip xs ys)

mapValueM :: Eq a => a -> (b->b) -> Map a b -> Map a b
mapValueM x f = map (\(y,v) -> if x==y then (y, f v) else (y,v))

-- Graphs --

type V = Int  -- always >= 0
data G = G (Map V [V])

nodes :: G -> [V]
nodes (G m) = nub $ map fst m

mkGraph :: [(V,[V])] -> G
mkGraph = G . nubBy (\x y -> fst x == fst y)

printGraph :: G -> IO()
printGraph (G l) =
  let p (v, l) =
        putStrLn $ show v ++ " |-> " ++ show l
  in mapM_ p l

filterG :: (V -> [V] -> Bool) -> G -> [V]
filterG f (G m) = map fst $ filter (uncurry f) m

foldrG :: (V -> [V] -> a -> a) -> a -> G -> a
foldrG f acc (G m) =
  foldr g acc m where
  g (v,vs) acc = f v vs acc

