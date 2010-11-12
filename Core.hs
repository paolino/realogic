{-# LANGUAGE  	ViewPatterns, ExistentialQuantification, 
		UnicodeSyntax, DeriveFoldable, DeriveFunctor, 
		DeriveTraversable, TupleSections 
		#-}

-- | A persistent Reactor is an object to mantain a bunch of stateful reactions. The reactor is reprogrammable via external events which are broadcasted to the reactions. Reaction intercommunication is left for the inside monad, which is requested to expose a state which contains the communication common data. Internal events which are used for untyped communications among reactions don't need to be serializable. 
-- On response to events reactions must reproduce themselves or die and can produce new reactions, and internal events.
-- A reactor exposes its internals to be saved and a resuming funcion from its internals. This is why it's persistent. Its internals should contain the minimum set of ordered external events, along with monad state and reactions internals, necessary to restore a fresh reactor wrapped around the right set of initial reactions to its actual reactive behaviour.

module Core -- (Rea (⋯), Serial (⋯), Recover, External, Internal, Reactor (⋯) , Response (⋯), makeReactor, parseSerialReactor) where
	where
import Prelude hiding (foldr, mapM, concat)
import Data.List (nub)
import Data.Maybe (mapMaybe, catMaybes, listToMaybe)
import Data.Traversable (Traversable, mapM, mapAccumL, forM) 
import Data.Foldable (Foldable, foldr, concat)
import Data.Typeable (Typeable, cast)
import Control.Monad (replicateM, foldM) 
import Control.Monad.Trans (lift) 
import Control.Monad.State (StateT, runStateT, MonadState, get , put) 
import Control.Monad.Writer (WriterT, runWriterT, tell, listen) 
import Control.Applicative ((<$>))
import Control.Arrow (second)
import Debug.Trace

import DepGraph (DepGraph (⋯), makeDG)
import Untypeds (Serial (Serial), Untyped (Untyped), casting, parseSerial, ParseSerial)


-- | Internal events are untyped, and not necessarily serializable
type Internal = Untyped

-- | external events are untyped, and must be serializable
type External = Serial

-- | internal state serializations for reactions are untyped
type Recover = Serial

-- | the value Rea objects must return 
data Response m = Response 
	Bool 	-- ^ the new reaction for this reaction, or False if reaction if dead
	[Rea m] 		-- ^ a list of new reactions
	[Internal] 		-- ^ some events to broadcast

-- | Rea is the main interface to this module. It's exixtentially polytyped in the type of value to react. This polytyping is wanted to support compile time plugins, not sharing their event types.
data Rea m  = ∀ a b . (Typeable a, Show b, Read b, Typeable b) ⇒ Rea 
	{	reaCtion ∷ a → StateT b m (Response m) 		-- ^ the actual reaction to events of type `a`
	,	reaState ∷ b			-- ^ the internal state of the reaction , everything closed
	}

stepRea ∷ (Monad m, Functor m) ⇒ Rea m → Internal → Maybe (m ([Rea m],[Internal], Maybe (Rea m)))
stepRea (Rea f b) (Untyped x) = k <$> cast x where
	k x = do
		(Response t xs ys, b') ← runStateT (f x) b
		return (xs,ys, if t then Just $ Rea f b' else Nothing)
		
	
	
	
-----------------------------------------------

-- the structure holding reactions
data Node a = Node a [Node a]  deriving (Foldable, Functor, Traversable)


prune ∷ (a → Bool) → Node a → Maybe (Node a)
prune c n@(Node x []) = if c x then Nothing else Just n
prune c n@(Node x ns) = let ns' = mapMaybe (prune c) ns in 
	if null ns' && c x then Nothing else Just (Node x ns')


data Core m i = Core {
	borned ∷ Maybe i,
	dead_or_alive ∷ Either i (Rea m)
	}


react' ∷ (Functor m, Monad m) ⇒ (i,Either External Internal) → Node (Core m i) →  WriterT [Internal] m (Node (Core m i))
react' e (Node c@(Core _ (Left _)) ns) = Node c <$> mapM (react' e) ns
react' e@(i, Right u) (Node (Core borned (Right r)) ns ) = do
	let 	new m = Node (Core (Just i) $ Right m) []
	ns' ← mapM (react' e) ns
	(ad,ns'') ← case stepRea r u of
		Nothing → return $ (Right r, ns')
		Just k → do 
			(ms',es,mrea) ← lift k
			tell es
			return $ (maybe (Left i) Right mrea, ns' ++ map new ms')
	return $  Node (Core borned ad) ns''
react' e@(i, Left (Serial x)) n = react' (i, Right (Untyped x)) n

react ∷ (Monad m, Functor m) ⇒   (i,External) → Node (Core m i) → m (Maybe (Node (Core m i)))
react (i,e) n = fmap fst . runWriterT . z (Just n) $ (i, Left e) where
	pruneReact e n = prune isDead <$> react' e n where
		isDead (Core _ (Left _)) = True
		isDead _ = False
	z Nothing _ = return Nothing
	z (Just n) e = do
		(n',map Right → es) ← listen $ pruneReact e n
		if null es then return n' else case n' of
			Nothing → return Nothing -- information leak
			n'' → foldM z n'' $ zip (repeat i) es

context ∷ Core m i → (Maybe Recover,[i])
context (Core (Just i) (Right (Rea _ v))) = (Just (Serial v),[i])
context	(Core (Just i) (Left j)) = (Nothing, [i,j])
context	(Core Nothing (Left j)) = (Nothing, [j])
context (Core Nothing (Right (Rea _ v))) = (Just (Serial v), [])

contextualize ∷ Eq i ⇒ Node (Core m i) → ([Maybe Recover],[i])
contextualize n = let (rs,is) = unzip . map context $ foldr (:) [] n in (rs,nub . concat $ is)

poke (Just (Serial x) :xs) (Core bnd (Right (Rea f b))) = case cast x of
	Nothing → error "Recover type corrupt"
	Just b' → (xs,Core bnd (Right (Rea f b')))
poke (Nothing : xs) c = (xs,c)

pokeNode (mr,n) = snd . mapAccumL poke mr $ n
-----------------------------------------------------------------------------
type ReactionStates = [[Maybe Recover]]

parseReactionStates :: ParseSerial ReactionStates
parseReactionStates ps mrs = 
	forM mrs $ \rs -> forM rs $ maybe (Just Nothing) (fmap Just . ps)

type SCore c = (c,External,[[Maybe Recover]])

type CoreDG c = DepGraph (SCore c) 

type Index = Int
insertCoreDG ∷ c → External → [Node (Core m Index)] → CoreDG c → (Index,CoreDG c)
insertCoreDG c e xs (DepGraph add _ _) = let (mrs,iss) = unzip $ map contextualize xs in 
		add ((c,e,mrs),nub . concat $ iss)

purgeCoreDG ∷ [Node (Core m Index)] →  CoreDG c → Maybe (CoreDG c)
purgeCoreDG ns (DepGraph _ resize _) = resize (nub . concat . map (snd . contextualize) $ ns)

parseSCore :: ParseSerial (SCore c)
parseSCore ps (c,e,mrs) = do
	e <- ps e
	mrs <- parseReactionStates ps mrs
	return (c,e,mrs)
----------------------------------------------------------------------------
type SerialReactor c = (ReactionStates,[SCore c])

parseSerialReactor :: ParseSerial (SerialReactor c)
parseSerialReactor ps (mrs,ss) = do
	mrs <- parseReactionStates ps mrs
	ss <- mapM (parseSCore ps) ss
	return (mrs,ss)

-- | operative interface to use some evolving reactions
data Reactor m c = Reactor {
	-- | update itself digesting a new event
	insertExternals ∷ [External] → m (Maybe (Reactor m c)), 
	-- | restore to a given serialization
	restoreReactor ∷ SerialReactor c → m (Maybe (Reactor m c)),
	-- | expose its internals, for later restoring
	serializeReactor ∷ SerialReactor c
	}

makeReactor ∷ ∀ m c . (Functor m, MonadState c m) ⇒ [Rea m] → Maybe (Reactor m c)
makeReactor rs = new zero where

	zeroNode r =  Node (Core Nothing (Right r)) []

	zero = (makeDG,map zeroNode rs)	

	new (dg,ns) = case ns of
		[] → Nothing
		_ → Just $ Reactor (insert ns dg') restore (map (fst . contextualize) ns, values dg') 
		where dg' = case purgeCoreDG ns dg of
				Nothing → error "fallimento nella riduzione del grafo di ripristino"
				Just dg' → dg'

	restore (actual,ecs) = new <$> second (ctxz actual) <$> foldM k zero ecs where
		ctxz mrs ns = map pokeNode $ zip mrs ns
		k (dg,ns) (c,e,mrs) = if length mrs /= length ns then error "restore fallito sullo zip" else do 
			put c 
			insert' (dg,ctxz mrs ns) e

	insert ns dg es = new <$> foldM insert' (dg,ns) es
	
		
	insert' (dg,ns) e = do
		c ← get
		let (i,dg') = insertCoreDG c e ns dg
		ns' ← catMaybes <$> mapM (react (i,e)) ns 
	 	return (dg' ,ns') 

--------------------------------------------------------------------------------------------

	
