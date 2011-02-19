
{-# LANGUAGE ViewPatterns #-}
-- | Resizable dependency graph.
module Data.Reactor.MinimalGraph (Index, MinimalGraph (..), mkMinimalGraph, prop_data_reactor_minimalgraph) where

import Data.List ((\\), union, nub,sortBy)
import Data.Ord (comparing)
import Control.Applicative ((<$>))


import Test.QuickCheck hiding (resize)
import Data.List (sort)
import Data.Maybe (mapMaybe)
import Control.Monad (foldM)

type Index= Int

-- internal rapresentation of a dependency graph. b is the key.
type Graph a b = [(b, (a,[b]))]

-- riduce il grafo affinche contenga solo gli elementi richiesti e le loro dipendenze
correct :: Eq b 
	=> Graph a b 		-- ^ grafo iniziale 
	-> [b] 				-- ^ elementi che ci devono essere con le loro dipendenze
	-> Maybe (Graph a b)	-- ^ grafo chiuso sulle dipendenze oppure Nothing se manca una dipendenza
correct h ts = do 	ds <- minimals' [] ts 
			zip ds <$> look ds
	where
	look =  mapM (`lookup` h)
	minimals' xs [] = Just xs
	minimals' xs ys = nub . concatMap snd <$> look zs >>= minimals' (xs ++ zs)
		where 	zs = ys \\ xs



-- | MinimalGraph object definition. Add and resisze are splitted to permit new index to be used in computation of new constraint group .
data MinimalGraph a = MinimalGraph
	{	add :: (a,[Index]) -> (Index,MinimalGraph a) 	-- ^ append a new value given its minimalendencies. Return its index
	,	resize :: [Index] -> Maybe (MinimalGraph a) 	-- ^ possibly reduce the object as to contain only the subgraph
	, 	values :: [a]		-- ^ elements in the graph
	}


-- | Create an empty minimal graph.
mkMinimalGraph :: MinimalGraph a
mkMinimalGraph  =  create 0 [] where
	create n xs = MinimalGraph (add' n xs) (resize' n xs) (map (fst.snd) $ sortBy (comparing fst) xs)
	add' n xs x = (n , create (n + 1) ((n,x):xs))
	resize' n xs ys = create n <$> correct xs ys

-------------- quick check prop ----------------------------------------

prop_data_reactor_minimalgraph :: Gen Bool
prop_data_reactor_minimalgraph = all id `fmap` sequence [coherent]

unions :: Eq a => [[a]] -> [a]
unions = foldr union []

coherent :: Gen Bool
coherent = do
	top <- elements [0..500::Int]
	let  k (rs,dg) x = do
		let ts = unions $ map snd rs
		zs <- if null ts then return [] else nub <$> listOf (elements ts)
		let (j,dg') = add dg (x,zs)
		return $ (((j ,x),zs):rs, dg')
	(qs,dg) <- foldM k ([],mkMinimalGraph) [0..top]
	
	ss <- listOf (elements qs) 
	let  lk = mapMaybe (flip lookup $ map fst qs)
	case resize dg (map (fst . fst) ss) of
		Nothing -> return (null ss)
		Just dg' -> return $ sort (values dg' ) == sort (unions $ map (snd . fst) ss : map (lk . snd) ss )


