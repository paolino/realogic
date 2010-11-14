
{-# LANGUAGE   DeriveFoldable, DeriveFunctor, 
		DeriveTraversable		#-}

module Data.Reactor.Node (prune, expandM, mkNode, Node (..)) where

import Data.Maybe (mapMaybe)
import Data.Traversable (Traversable)
import Data.Foldable (Foldable)
-----------------------------------------------

-- | the structure holding the reactions. A tree structure is needed to track the reactions belonging. The tree make it possible to eliminate lines of dead reactions.
data Node a = Node 
	a
	[Node a]	
	  deriving (Foldable, Functor, Traversable)

-- | dead branches elimination, prune compute Nothing if the value and all the subvalues are judjed false 
prune 	:: (a -> Bool)  	-- ^ judging function
	-> Node a 		-- ^ node to judge
	-> Maybe (Node a)	-- ^ the node judged with judged sons
prune c n@(Node x []) = if c x then Nothing else Just n
prune c (Node x ns) = let ns' = mapMaybe (prune c) ns in 
	if null ns' && c x then Nothing else Just (Node x ns')

-- | monadic node expansion
expandM :: (Functor m, Monad m) => (a -> m (a,[a])) -> Node a -> m (Node a)
expandM f (Node x ns) = do
	(x',xs') <- f x
	ns' <- mapM (expandM f) ns
	return $ Node x' (ns' ++ map (flip Node []) xs')

mkNode :: a -> Node a
mkNode x = Node x []
--------------------------------------------------------------------------

