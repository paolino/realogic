
{-# LANGUAGE   DeriveFoldable, DeriveFunctor, 
		MultiParamTypeClasses, DeriveTraversable, NoMonomorphismRestriction		#-}

module Data.Reactor.Pruned   -- (mkPruned , Pruned (..),  prop_data_reactor_pruned) 
	where

import Data.Maybe (mapMaybe)
import Data.Traversable (Traversable, mapAccumL)
import Data.Foldable (Foldable, toList)

import Test.QuickCheck
import Control.Monad.Identity
import Debug.Trace

-----------------------------------------------


--  the structure holding the reactions. A tree structure is needed to track the reactions belonging. The tree make it possible to eliminate lines of dead reactions.
data Node a = Node {
	value :: a,
	sons :: [Node a]	
	}
	  deriving (Foldable, Functor, Traversable)

--  dead branches elimination, prune compute Nothing if the value and all the subvalues are judjed false 
prune 	:: (a -> Bool) 
	-> Node a 		-- ^ node to judge
	-> Maybe (Node a)	-- ^ the node judged with judged sons
prune f n@(Node x []) =  if f x then Nothing else Just n
prune f (Node x ns) = let ns' = mapMaybe (prune f) ns in 
	if null ns' && f x then Nothing else Just (Node x ns')

--  monadic node expansion
expandM :: (Monad m) 
	=> (a -> m (a,[a]))
	-> Node a 
	-> m (Node a)
expandM f  (Node x ns) = do
	(x',xs') <- f x 
	ns' <- mapM (expandM f ) ns
	return $  Node x' (ns' ++ map (flip Node []) xs')

serializeNode :: (a -> b ) -> Node a -> [b]
serializeNode f = map f . toList

restoreNode :: (a -> b -> a) -> Node a -> [b] -> Node a
restoreNode f n xs = snd . mapAccumL poke xs $ n where
	poke [] _ = error "Serialization shorter than tree"
	poke (x:xs) y = (xs, f y x)

-- | A pruned object keeps a pruned tree of values
data Pruned m b = Pruned	{
	expand :: m (Maybe (Pruned m b)), -- ^ expand the pruned , Nothing if nothing in it at the end
	serialize :: [b],		-- ^ collect the pruned values
	restore :: [b] -> Pruned m b	-- ^ restore the pruned given it's values
	}
-- | build a pruned given some functions and a value 
mkPruned 	:: (Monad m ) 
		=> (a -> m (a,[a])) -- ^ monadic value expansion, first value is the update , second values are dependents
		-> (a -> Bool)	-- ^ pruning condition
		-> (a -> b -> a) -- ^ restoring function via parameter 
		-> (a -> b) 	-- ^ serialization to parameter
		-> a 		-- ^ value for the base node
		-> Pruned m b 	-- ^ the pruned object
mkPruned expand' prune' restore' serialize' x = new $ Node x [] where
	new x = Pruned (exp x) (serializeNode serialize' x) (new . restoreNode restore' x)
	exp x = do
		x' <- expandM expand' x 
		case prune prune' x' of
			Nothing -> return Nothing
			Just x'' -> return (Just . new $ x'')


------------ Quick check properties --------------------------------------------------------------

prop_data_reactor_pruned = all id `fmap` sequence [prop_node_expand, prop_node_prune, prop_node_restore]

prop_node_expand :: Gen Bool
prop_node_expand = do
	let 	testI = \() -> return (() , [()])
	r <- elements [0..13]
	let n' = (!!r) $ iterate (runIdentity . expandM testI) $ Node () []
	return $ length (toList n') == 2 ^ r

prop_node_prune = do	
	let 	u = 10 
		k x = do
			y <- elements [0..u]
			return (x,[y])
	let 	subtrees n@(Node x []) = [n]
		subtrees n@(Node x ns) = n:ns
	n' <- foldM (\n _ -> expandM k  n) (Node 0 []) [0..u]
	t <- elements [0..u]
	let 	q c = case  prune c n' of
			Nothing -> True
			Just n''' ->  let sts = filter (\(Node x _) -> c x ) $ subtrees n'''
				in  all (\n -> length (filter (not . c) . toList $ n) > 0) $ sts
	return $ q (>t) && q (<t) && q (==t)
prop_node_restore = do	
	let 	u = 10 
		k x = do
			y <- elements [0..u]
			return (x,[y])
	let 	subtrees n@(Node x []) = [n]
		subtrees n@(Node x ns) = n:ns
	n' <- foldM (\n _ -> expandM k n) (Node 0 []) [0..u]
	return $ toList (restoreNode const n' . serializeNode id $ n') == toList n'

