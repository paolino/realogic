{-# LANGUAGE ScopedTypeVariables #-}
-- | A reactor is an object to hold a bunch of reactions, which create other reactions and events and respond to events.
module Data.Reactor   (Reactor (..), mkReactor) where

import Data.List (nub)
import Data.Maybe (fromMaybe, catMaybes)

import Control.Applicative ((<$>))

import Control.Monad (foldM, ap) 

import Control.Monad.State (MonadState, get, put)
import Control.Monad.Writer (runWriterT, listen) 
import Control.Monad.Reader (local, runReaderT)

import Control.Arrow (second)

import Data.Reactor.Untypeds (toUntyped, Serial)
import Data.Reactor.Reaction ( Reaction)
import Data.Reactor.MinimalGraph (Index, MinimalGraph(..), mkMinimalGraph)
import Data.Reactor.Pruned (Pruned (..), expand, serialize, restore)
import Data.Reactor.Operational  (OperationalLayer, Operational (..), mkOperationalPruned)
import Data.Reactor.Serialization (Serialization, SerialReactor)

-- a Pruned object for Operationals
type ReaTree m = Pruned (OperationalLayer m) (Maybe Serial,[Index])


-- | The reactor object. Once created this closures control its lifecycle. Updaters return Nothing when the reactor is wrapped around no reactions.
data Reactor m c = Reactor {
	-- | Update itself digesting a new event
	insertExternals :: [Serial] -> m (Maybe (Reactor m c)), 
	-- | Regenerate itself from a serialization
	restoreReactor :: SerialReactor c -> m (Maybe (Reactor m c)),
	-- | Serialize its internals, for later restoring.
	serializeReactor :: SerialReactor c
	}

serialize' :: ReaTree m -> ([Maybe Serial], [Index])
serialize'  = second (concat) . unzip . serialize

insertSerialization 	:: c 		--  reactor global state
			-> Serial 	--  event to happen 
			-> [ReaTree m] 	--  reaction trees
			-> Serialization c --  serialization object
			-> (Index,Serialization c) --  updated object

insertSerialization c e xs (MinimalGraph add' _ _) = let (mrs,iss) = unzip $ map serialize' xs in 
		add' ((c,e,mrs),nub . concat $ iss)

purgeSerialization :: [ReaTree m] ->  Serialization c -> Maybe (Serialization c)
purgeSerialization ns (MinimalGraph _ resize' _) = resize' (nub . concat . map (snd . serialize') $ ns)



-- wrap a reaction 
mkReaTree ::(Monad m, Functor m) =>  Reaction m -> ReaTree m
mkReaTree r = mkOperationalPruned $ Operational Nothing (Right r) 

-- create the reactor object closure
zero :: (Monad m, Functor m) => [Reaction m] -> (Serialization c,[ReaTree m])
zero rs = (mkMinimalGraph, map mkReaTree rs)	
-- create a reactor object from the closure. Fails if the ReaTree list is empty, which means all reactions are over
new :: (Functor m, MonadState c m) => [Reaction m] -> (Serialization c,[ReaTree m]) -> Maybe (Reactor m c)
new rs (dg,ns) = case ns of
	[] -> Nothing
	_ -> Just $ Reactor (insert rs (dg',ns)) (restore' rs) (map (fst . serialize') ns, values dg') 
	where dg' = fromMaybe 
			(error "Restore graph reduction failed")
			$ purgeSerialization ns dg 

-- create a new reactor restoring from a serialization
restore' :: forall m c . (Functor m, MonadState c m) => [Reaction m] -> SerialReactor c -> m (Maybe (Reactor m c))
restore' rs (actual,ecs) = new rs <$> second (ctxz actual) <$> foldM k (zero rs) ecs where
	ctxz :: [[Maybe Serial]] -> [ReaTree m] -> [ReaTree m]
	ctxz mrs ns = map (\(mr,n) -> restore n $ zip mr $ error "restoring dependencies") $ zip mrs ns
	k (dg,ns) (c,e,mrs) = if length mrs /= length ns 
		then error "Restore failed in the numbers of base reactions" 
		else do put c 
			insert' (dg,ctxz mrs ns) e

-- create a reactor from the closure inserting some events
insert :: (Functor m, MonadState c m) => [Reaction m] -> (Serialization c,[ReaTree m]) -> [Serial] -> m (Maybe (Reactor m c))
insert rs (dg,ns) es = new rs <$> foldM insert' (dg,ns) es

-- insert one event updating the closure
insert' :: (Functor m, MonadState c m) => (Serialization c,[ReaTree m]) -> Serial -> m (Serialization c,[ReaTree m])
insert' (dg,ns) e = do
	c <- get
	let (i,dg') = insertSerialization c e ns dg
	ns' <- catMaybes <$> mapM (react (i,e)) ns 
	return (dg' ,ns') 

-- core reaction. Consumes all events , the firestarter and recursively all the produced events
react :: (Monad m, Functor m) =>   (Index,Serial) -> ReaTree m  -> m (Maybe (ReaTree m))
react (i,e) p = fmap fst . runWriterT . flip runReaderT (i,toUntyped e) $ z (Just p) where
	z Nothing  = return Nothing
	z (Just p') = do
		(p'',es) <- listen $ expand p'
		if null es then return p'' else case p'' of
			Nothing -> return Nothing -- information leak	
			p''' -> let k mp e' = local (\(i',_) -> (i',e')) $ z mp
				in foldM k p''' es

-- | build a reactor object  from a list of basic reactions
mkReactor :: (Functor m, MonadState c m) 
	=> [Reaction m] 	-- ^ list of base reactions
	-> Reactor m c	-- ^ a fresh reactor object

mkReactor = fromMaybe (error "cannot make a reactor with no base reactions") . (new `ap` zero)
