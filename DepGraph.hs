
{-# LANGUAGE ViewPatterns #-}
-- | Dependency graph
module DepGraph where

import Data.List ((\\), lookup, union, nub,sortBy)
import Data.Ord (comparing)
import Control.Applicative ((<$>))
import Control.Exception (assert)
import Test.QuickCheck
import Debug.Trace
import Control.Monad (replicateM)
import Data.Maybe


-- | insieme di elementi di tipo a indicizzati da b e legati alle loro dipendenze, ovvero grafo delle dipendenze
type DepGraph' a b = [(b, (a,[b]))]

-- | riduce il grafo affinche contenga solo gli elementi richiesti e le loro dipendenze
correct :: Eq b 
	=> DepGraph' a b 		-- ^ grafo iniziale 
	-> [b] 				-- ^ elementi che ci devono essere con le loro dipendenze
	-> Maybe (DepGraph' a b)	-- ^ grafo chiuso sulle dipendenze oppure Nothing se manca una dipendenza
correct h ts = do 	ds <- deps' [] ts 
			zip ds <$> look ds
	where
	look =  mapM (`lookup` h)
	deps' xs [] = Just xs
	deps' xs ys = nub . concatMap snd <$> look zs >>= deps' (xs ++ zs)
		where 	zs = ys \\ xs

-- | controlla la coerenza di grafo, tutte le dipendenze dell'insieme sono nell'insieme
coherent :: Eq b => DepGraph' a b -> Bool
coherent (unzip -> (is, unzip -> (_,js))) = all (`elem` is) . foldr union [] $ js
	
testDephGraph' = do
	n <- elements [1..100]
	let  	ns = [1..n]
		fromNs = elements ns >>= \m -> replicateM m (elements ns)
	g <- zip ns <$> zip (repeat ()) <$> replicateM n fromNs
	maybe False coherent . correct g <$> fromNs

-- | DepGraph object definition 
data DepGraph a = DepGraph
	{	add :: (a,[Int]) -> (Int,DepGraph a) 	-- ^ append a new value given its dependencies. Return its index
	,	resize :: [Int] -> Maybe (DepGraph a) 	-- ^ possibly reduce the object as to contain only the subgraph
	, 	values :: [a]		-- ^ expose internals
	}

-- | DepGraph object constructor
makeDG :: DepGraph a
makeDG = restore [] 

-- | restore the object from its internals
restore :: DepGraph' a Int -> DepGraph a
restore dg' = assert (coherent dg') $ create ((+1) . maximum . (-1:) $ map fst dg') dg' where
	create n xs = DepGraph (add' n xs) (resize' n xs) (map (fst.snd) $ sortBy (comparing fst) xs)
	add' n xs x = (n + 1, create (n + 1) ((n,x):xs))
	resize' n xs ys = create n <$> correct xs ys
