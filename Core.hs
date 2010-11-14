{-# LANGUAGE  	ExistentialQuantification, 
		UnicodeSyntax, DeriveFoldable, DeriveFunctor, 
		DeriveTraversable, TupleSections, TypeFamilies,
		MultiParamTypeClasses
		#-}

-- | A persistent Reactor is an object to mantain a bunch of stateful reactions. The reactor is reprogrammable via external events which are broadcasted to the reactions. Reaction intercommunication is left for the inside monad, which is requested to expose a state which contains the communication common data. Internal events which are used for untyped communications among reactions don't need to be serializable. 
-- On response to events reactions must reproduce themselves or die and can produce new reactions, and internal events.
-- A reactor exposes its internals to be saved and a resuming funcion from its internals. This is why it's persistent. Its internals should contain the minimum set of ordered external events, along with monad state and reactions internals, necessary to restore a fresh reactor wrapped around the right set of initial reactions to its actual reactive behaviour.

module Core  (Rea (..), Serial (..), Recover, External, 
		Internal, Reactor (..) , Response (..), makeReactor, parseSerialReactor) 
	where

import Prelude hiding (foldr, mapM, concat)
import Data.List (nub)
import Data.Maybe (fromMaybe, mapMaybe, catMaybes)
import Data.Traversable (Traversable, mapM, mapAccumL, forM) 
import Data.Foldable (Foldable, foldr, concat)
import Data.Typeable (Typeable, cast)
import Control.Monad (foldM) 
import Control.Monad.Trans (lift) 
import Control.Monad.State (StateT, runStateT, MonadState, get , put) 
import Control.Monad.Writer (WriterT, runWriterT, tell, listen) 
import Control.Monad.Reader (ReaderT, runReaderT, ask)
import Control.Applicative ((<$>))
import Control.Arrow (second)

import DepGraph (DepGraph (..), makeDG)
import Untypeds (Serial (Serial), Untyped (Untyped), ParseSerial, toUntyped)


-- | Internal events are untyped, and not necessarily serializable
type Internal = Untyped

-- | external events are untyped, and must be serializable
type External = Serial

-- | internal state serializations for reactions are untyped
type Recover = Serial

-- | The value Rea objects must return.
data Response m = Response 
	Bool 			-- ^ True to continue the reaction, or False if reaction if dead
	[Rea m] 		-- ^ a list of new reactions, just borned ready for the next event
	[Internal] 		-- ^ some events to broadcast now as effects of the reaction

-- | Rea is the main interface to this module. It's exixtentially typed in the type of value to react and in its internal state. This polytyping is wanted to support compile time plugins.
data Rea m  = forall a b . (Typeable a, Show b, Read b, Typeable b) => Rea 
	-- | the next reaction to an event of type 'a'. The reaction can modify its individual state in the outer 
	-- monad layer. There is no constraint on the inner monad.
	{	reaCtion :: a -> StateT b m (Response m) 		-- ^ the actual reaction to events of type `a`
	,	reaState :: b			-- ^ the internal state of the reaction , everything closed
	}

class ReaC m a where
	data ReaK m a :: * -> * 
	reaction :: ReaK m a b -> a -> StateT b m (Response m)
	reastate :: ReaK m a b -> b


-- try a reaction of a Rea, given an event presented in an Untyped box. If the event is not of the right type, the result is Nothing, otherwise an action in the monad m returning a modified Response, with Bool mapped to Maybe (Rea m).
stepRea :: (Monad m, Functor m) 
	=> Rea m 		-- reaction to try
	-> Internal 		-- event
	-> Maybe (m 	(
			[Rea m], -- new reactors
			[Internal], -- new events
			Maybe (Rea m) -- possibly a renewed rea, or Nothing if the reaction is dead
			)
		)
stepRea (Rea f b) (Untyped x) = k <$> cast x where
	k x' = do
		(Response t xs ys, b') <- runStateT (f x') b
		return (xs,ys, if t then Just $ Rea f b' else Nothing)
		
	
	
	
-----------------------------------------------

-- the structure holding the reactions. A tree structure is needed to track the reactions belonging. The tree make it possible to eliminate lines of dead reactions.
data Node a = Node a [Node a]  deriving (Foldable, Functor, Traversable)

-- dead branches elimination
prune :: (a -> Bool) -> Node a -> Maybe (Node a)
prune c n@(Node x []) = if c x then Nothing else Just n
prune c (Node x ns) = let ns' = mapMaybe (prune c) ns in 
	if null ns' && c x then Nothing else Just (Node x ns')

--------------------------------------------------------------------------
-- the value stored in the tree nodes
data Core m i = Core 
	(Maybe i) -- Nothing if the rea was of the boot set, an index to the contextualized event which borned it
	(Either i (Rea m)) -- the reaction if alive or an index to the last event reacted to
	

-- single event reaction of a reactor branch 
react' :: (Functor m, Monad m) => Internal -> Node (Core m i) ->  ReaderT i (WriterT [Internal] m) (Node (Core m i))
react' e (Node c@(Core _ (Left _)) ns) = Node c <$> mapM (react' e) ns
react' e (Node (Core borned (Right r)) ns) = do
	i <- ask
	let 	new m = Node (Core (Just i) $ Right m) []
	ns' <- mapM (react' e) ns
	(ad,ns'') <- case stepRea r e of
		Nothing -> return (Right r, ns')
		Just k -> lift $ do 
			(ms',es,mrea) <- lift k
			tell es
			return (maybe (Left i) Right mrea, ns' ++ map new ms')
	return $  Node (Core borned ad) ns''

pruneReact ::	(Functor m, Monad m) 
	=> Internal
	-> Node (Core m i)
	-> ReaderT i (WriterT [Internal] m) (Maybe (Node (Core m i)))
pruneReact e n = prune isDead <$> react' e n where
	isDead (Core _ (Left _)) = True
	isDead _ = False

react :: (Monad m, Functor m) =>   (i,External) -> Node (Core m i) -> m (Maybe (Node (Core m i)))
react (i,e) n = fmap fst . runWriterT . flip runReaderT i . z (Just n) $ toUntyped e where
	z Nothing _ = return Nothing
	z (Just n') e' = do
		(n'',es) <- listen $ pruneReact e' n'
		if null es then return n'' else case n'' of
			Nothing -> return Nothing -- information leak
			n''' -> foldM z n''' es

context :: Core m i -> (Maybe Recover,[i])
context (Core (Just i) (Right (Rea _ v))) = (Just (Serial v),[i])
context	(Core (Just i) (Left j)) = (Nothing, [i,j])
context	(Core Nothing (Left j)) = (Nothing, [j])
context (Core Nothing (Right (Rea _ v))) = (Just (Serial v), [])

contextualize :: Eq i => Node (Core m i) -> ([Maybe Recover],[i])
contextualize n = let (rs,is) = unzip . map context $ foldr (:) [] n in (rs,nub . concat $ is)

poke ::		[Maybe Serial] 
		-> Core m i 
		-> ([Maybe Serial], Core m i)

poke (Just (Serial x) :xs) (Core bnd (Right (Rea f _))) = case cast x of
	Nothing -> error "Recover type corrupt"
	Just b' -> (xs,Core bnd (Right (Rea f b')))
poke (Just _ : _) _ = error "Recover a state to to dead reaction"
poke (Nothing : xs) c = (xs,c)
poke [] _ = error "Recover finished individual states too early"

pokeNode :: ([Maybe Serial], Node (Core m i)) -> Node (Core m i)
pokeNode (mr,n) = snd . mapAccumL poke mr $ n
-----------------------------------------------------------------------------
type ReactionStates = [[Maybe Recover]]

parseReactionStates :: ParseSerial ReactionStates
parseReactionStates ps mrs = 
	forM mrs $ \rs -> forM rs $ maybe (Just Nothing) (fmap Just . ps)

type SCore c = (c,External,[[Maybe Recover]])

type CoreDG c = DepGraph (SCore c) 

type Index = Int
insertCoreDG :: c -> External -> [Node (Core m Index)] -> CoreDG c -> (Index,CoreDG c)
insertCoreDG c e xs (DepGraph add' _ _) = let (mrs,iss) = unzip $ map contextualize xs in 
		add' ((c,e,mrs),nub . concat $ iss)

purgeCoreDG :: [Node (Core m Index)] ->  CoreDG c -> Maybe (CoreDG c)
purgeCoreDG ns (DepGraph _ resize' _) = resize' (nub . concat . map (snd . contextualize) $ ns)

parseSCore :: ParseSerial (SCore c)
parseSCore ps (c,e,mrs) = do
	e' <- ps e
	mrs' <- parseReactionStates ps mrs
	return (c,e',mrs')
----------------------------------------------------------------------------
type SerialReactor c = (ReactionStates,[SCore c])

parseSerialReactor :: ParseSerial (SerialReactor c)
parseSerialReactor ps (mrs,ss) = do
	mrs' <- parseReactionStates ps mrs
	ss' <- mapM (parseSCore ps) ss
	return (mrs',ss')

-- | operative interface to use some evolving reactions
data Reactor m c = Reactor {
	-- | update itself digesting a new event
	insertExternals :: [External] -> m (Maybe (Reactor m c)), 
	-- | restore to a given serialization
	restoreReactor :: SerialReactor c -> m (Maybe (Reactor m c)),
	-- | expose its internals, for later restoring
	serializeReactor :: SerialReactor c
	}

makeReactor :: forall m c . (Functor m, MonadState c m) => [Rea m] -> Maybe (Reactor m c)
makeReactor rs = new zero where

	zeroNode r =  Node (Core Nothing (Right r)) []

	zero = (makeDG,map zeroNode rs)	

	new (dg,ns) = case ns of
		[] -> Nothing
		_ -> Just $ Reactor (insert ns dg') restore (map (fst . contextualize) ns, values dg') 
		where dg' = fromMaybe 
				(error "Restore graph reduction failed")
				$ purgeCoreDG ns dg 

	restore (actual,ecs) = new <$> second (ctxz actual) <$> foldM k zero ecs where
		ctxz mrs ns = map pokeNode $ zip mrs ns
		k (dg,ns) (c,e,mrs) = if length mrs /= length ns 
			then error "Restore failed in the numbers of base reactions" 
			else do 
				put c 
				insert' (dg,ctxz mrs ns) e

	insert ns dg es = new <$> foldM insert' (dg,ns) es
	
		
	insert' (dg,ns) e = do
		c <- get
		let (i,dg') = insertCoreDG c e ns dg
		ns' <- catMaybes <$> mapM (react (i,e)) ns 
	 	return (dg' ,ns') 

--------------------------------------------------------------------------------------------

	
