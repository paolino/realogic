import Test.QuickCheck
import Data.Maybe
import Data.List
import Data.Foldable (toList)
import Control.Monad.State
import Control.Monad.Identity
import Data.Typeable
import Control.Applicative ((<$>))

import Data.Reactor.DepGraph 
import Data.Reactor.Node
import Data.Reactor.Reaction 
import Data.Reactor.Untypeds

prop_node_expand :: Gen Bool
prop_node_expand = do
	r <- elements [0..13]
	let n' = (!!r) $ iterate (runIdentity . expandM (\() -> return (() , [()]))) $ mkNode ()
	return $ length (toList n') == 2 ^ r

prop_node_prune = do	
	let 	subtrees n@(Node x []) = [n]
		subtrees n@(Node x ns) = n:ns
	let 	u = 10 
		k x = do
			y <- elements [0..u]
			return (x,[y])
	n' <- foldM (\n _ -> expandM k n) (mkNode 0) [0..u]
	t <- elements [0..u]
	let 	q c = case  prune c n' of
			Nothing -> True
			Just n''' ->  let sts = filter (\(Node x _) -> c x ) $ subtrees n'''
				in  all (\n -> length (filter (not . c) . toList $ n) > 0) $ sts
	return $ q (>t) && q (<t) && q (==t)

prop_depGraph :: Gen Bool
prop_depGraph = do
	top <- elements [0..100]
	let unions = foldr union []
	    k (rs,dg) x = do
		let ts = unions $ map snd rs
		zs <- if null ts then return [] else nub <$> listOf (elements ts)
		let (j,dg') = add dg (x,zs)
		return $ (((j,x),zs):rs, dg')
	(qs,dg) <- foldM k ([],makeDG) [0..top]
	ss <- listOf (elements qs)
	case Data.Reactor.DepGraph.resize dg (map (fst . fst) qs) of
		Nothing -> return False
		Just dg' -> return $ sort (values dg' ) == sort (unions $ (map (snd . fst) qs): map snd qs )

prop_rea = do
	ms <- listOf (elements [1..10::Int])
	let 	r = Rea (\y -> modify (+ y) >> return (Response True [] [])) (0::Int) 
		ck Nothing  _ = return Nothing
		ck (Just r) x = do 
				let k = stepRea r x
				case k of 
					Nothing -> return Nothing
					Just k' -> do
						(_,_,mr) <- k'
						return mr
		ef = foldM ck (Just r) $ map Untyped ms
		q = case runIdentity ef of 
			Just (Rea _ z) -> case cast z of
				Just z' ->  z' == sum ms
				Nothing -> False
			Nothing -> False
	return q

main = mapM_ (quickCheckWith (Args {replay = Nothing, maxSuccess = 200, maxDiscard = 500, maxSize = 100})) 
	[	prop_depGraph,
		prop_rea,
		prop_node_expand,
		prop_node_prune
	]
