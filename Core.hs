{-# LANGUAGE  ViewPatterns, ExistentialQuantification,  ScopedTypeVariables, UnicodeSyntax, DeriveFoldable, DeriveFunctor, DeriveTraversable, TupleSections #-}
module Core (Rea (⋯), Serial (..), Recover, Event, Reactor (⋯) , Response (⋯), makeReactor) where

import Prelude hiding (foldr, mapM, concat)
import Data.List (nub)
import Data.Maybe (mapMaybe, catMaybes, listToMaybe)
import Data.Traversable (Traversable, mapM, mapAccumR) 
import Data.Foldable (Foldable, foldr, concat)
import Data.Typeable (Typeable, cast)
import Control.Monad (replicateM, foldM) 
import Control.Monad.Trans (lift) 
import Control.Monad.State (MonadState, get , put) 
import Control.Monad.Writer (WriterT, runWriterT, tell, listen) 
import Control.Applicative ((<$>))

import DepGraph (DepGraph (⋯), makeDG)
import Serial (Serial (Serial))



-- | events are untyped
type Event = Serial

-- | internal state serializations for reactions are untyped
type Recover = Serial

-- | the value Rea objects must return 
data Response m = Response 
	(Maybe (Rea m)) 	-- ^ the new reaction for this reaction, or Nothing if reaction if dead
	[Rea m] 		-- ^ a list of new reactions
	[Event] 		-- ^ some events to broadcast

-- | Rea is the main interface to this module. It's exixtentially polymorphic in the type of value to react.
data Rea m  = ∀ a . Typeable a ⇒ Rea 
	{	reaction ∷ a → m (Response m) 		-- ^ the actual reaction to events of type `a`
	,	reaState ∷ Recover			-- ^ the internal state of the reaction , everything closed
	,	reaRestore ∷ Recover → Maybe (Rea m)	-- ^ a restoring action for this Rea
	}

-----------------------------------------------

data Node a = Node a [Node a]  deriving (Foldable, Functor, Traversable)

prune ∷ (a → Bool) → Node a → Maybe (Node a)
prune c n@(Node x []) = if c x then Nothing else Just n
prune c n@(Node x ns) = let ns' = mapMaybe (prune c) ns in 
	if null ns' && c x then Nothing else Just (Node x ns')




data Core m i = Core {
	borned ∷ Maybe i,
	dead_or_alive ∷ Either i (Rea m)
	}

casting :: (Typeable a, Typeable c) => (a -> b) -> c -> Maybe b
casting f x = f <$> cast x

react' ∷ (Functor m, Monad m) ⇒ (i,Event) → Node (Core m i) →  WriterT [Event] m (Node (Core m i))
react' e (Node c@(Core _ (Left _)) ns) = Node c <$> mapM (react' e) ns
react' e@(i, Serial x) (Node (Core borned (Right r@(Rea f _ _))) ns ) = do
	let 	new m = Node (Core (Just i) $ Right m) []
	ns' ← mapM (react' e) ns
	(ad,ns'') ← case casting f x of 
		Nothing → return $ (Right r, ns')
		Just k → do 
			Response r' ms' es ← lift k	
			tell es
			return $ (maybe (Left i) Right r', ns' ++ map new ms')
	return $  Node (Core borned ad) ns''
react ∷ (Monad m, Functor m) ⇒   (i,Event) → Node (Core m i) → m (Maybe (Node (Core m i)))
react e@(i,_) n = fmap fst . runWriterT . z (Just n) $ e where
	pruneReact e n = prune isDead <$> react' e n where
		isDead (Core _ (Left _)) = True
		isDead _ = False
	z Nothing _ = return Nothing
	z (Just n) e = do
		(n',es) ← listen $ pruneReact e n
		if null es then return n' else case n' of
			Nothing → return Nothing -- information leak
			n'' → foldM z n'' $ zip (repeat i) es

context ∷ Core m i → (Maybe Recover,[i])
context (Core (Just i) (Right r)) = (Just (reaState r),[i])
context	(Core (Just i) (Left j)) = (Nothing, [i,j])
context	(Core Nothing (Left j)) = (Nothing, [j])
context (Core Nothing (Right r)) = (Just (reaState r), [])

contextualize ∷ Eq i ⇒ Node (Core m i) → ([Maybe Recover],[i])
contextualize n = let (rs,is) = unzip . map context $ foldr (:) [] n in (rs,nub . concat $ is)

poke (Just x :xs) (Core bnd (Right (Rea _ _ fromReco))) = case fromReco x of
	Nothing → error "Recover type corrupt"
	Just nr → (xs,Core bnd (Right nr))
poke (Nothing : xs) c = (xs,c)

pokeNode (mr,n) = snd . mapAccumR poke mr $ n
-----------------------------------------------------------------------------
type Index = Int

type SCore c = (c,Event,[[Maybe Recover]])
type CoreDG c = DepGraph (SCore c) 

insertCoreDG ∷ c → Event → [Node (Core m Index)] → CoreDG c → (Index,CoreDG c)
insertCoreDG c e xs (DepGraph add _ _) = let (mrs,iss) = unzip $ map contextualize xs in 
		add ((c,e,mrs),nub . concat $ iss)

purgeCoreDG ∷ [Node (Core m Index)] →  CoreDG c → Maybe (CoreDG c)
purgeCoreDG ns (DepGraph _ resize _) = resize (nub . concat . map (snd . contextualize) $ ns)

----------------------------------------------------------------------------
type SerialReactor c = [SCore c]

-- | operative interface to use some evolving reactions
data Reactor m c = Reactor {
	-- | update itself digesting a new event
	insertEvent ∷ Event → m (Reactor m c), 
	-- | restore itself given a serialization
	restoreReactor ∷ SerialReactor c → m (Reactor m c),
	-- | expose its internals, for later restoring
	serializeReactor ∷ SerialReactor c
	}

makeReactor ∷ ∀ m c . (Functor m, MonadState c m) ⇒ [Rea m] → Reactor m c
makeReactor rs = let
	new dg ns = let r = Reactor (insert ns dg) (restore ns dg) (values dg) in r
	restore ns dg ecs = uncurry new <$> foldM k (dg,ns) ecs where
		k (dg,ns) (c,e,mrs) = if length mrs == length ns then error "restore fallito sullo zip" else do 
			put c 
			let ns' = map pokeNode $ zip mrs ns
			insert' (dg,ns') e
	insert ns dg e = uncurry new <$> insert' (dg,ns) e 
	insert' (dg,ns) e = do
		c ← get
		let (i,dg') = insertCoreDG c e ns dg
		(dg' ,) . catMaybes <$> mapM (react (i,e)) ns  where
	zeroNode r =  Node (Core Nothing (Right r)) []
	in new makeDG (map zeroNode rs)

--------------------------------------------------------------------------------------------

	

