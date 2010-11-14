module Data.Reactor  (Reactor (..), makeReactor,  
	-- reexports
	External, Internal, Serial (..), Rea (..), 
	parseSerialReactor, Response (..)) where

import Data.Maybe (fromMaybe, catMaybes)
import Control.Monad (foldM, ap) 
import Control.Applicative ((<$>))
import Control.Monad.State (MonadState,get,put)
import Control.Arrow (second)

import Data.Reactor.Reaction (External)
import Data.Reactor.DepGraph (DepGraph, makeDG, values)
import Data.Reactor.Node (mkNode)
import Data.Reactor.OpNode (OpNode, react, contextualize, pokeNode)
import Data.Reactor.Operational (Operational (..))
import Data.Reactor.Serialization (Serialization, SerialReactor, insertSerialization, purgeSerialization)

-- reexports only ---------

import Data.Reactor.Reaction (Rea (..), Internal, Response (..))
import Data.Reactor.Serialization (parseSerialReactor)
import Data.Reactor.Untypeds (Serial (..))


-- | the operative interface to use some evolving reactions
data Reactor m c = Reactor {
	-- | update itself digesting a new event
	insertExternals :: [External] -> m (Maybe (Reactor m c)), 
	-- | restore to a given serialization
	restoreReactor :: SerialReactor c -> m (Maybe (Reactor m c)),
	-- | expose its internals, for later restoring
	serializeReactor :: SerialReactor c
	}


-- | build a reactor object  from a list of basic reactions
makeReactor :: (Functor m, MonadState c m) 
	=> [Rea m] 	-- ^ list of base reactions
	-> Reactor m c	-- ^ a fresh reactor object

makeReactor = fromMaybe (error "cannot make a reactor with no base reactions") . (new `ap` zero)

-- wrap a reaction 
zeroNode :: Rea m -> OpNode m
zeroNode r =  mkNode (Operational Nothing (Right r))

-- create the reactor object closure
zero :: [Rea m] -> (Serialization c,[OpNode m])
zero rs = (makeDG,map zeroNode rs)	

-- create a reactor object from the closure. Fails if the OpNode list is empty, which means all reactions are over
new :: (Functor m, MonadState c m) => [Rea m] -> (Serialization c,[OpNode m]) -> Maybe (Reactor m c)
new rs (dg,ns) = case ns of
	[] -> Nothing
	_ -> Just $ Reactor (insert rs (dg',ns)) (restore rs) (map (fst . contextualize) ns, values dg') 
	where dg' = fromMaybe 
			(error "Restore graph reduction failed")
			$ purgeSerialization ns dg 

-- create a new reactor restoring from a serialization
restore :: (Functor m, MonadState c m) => [Rea m] -> SerialReactor c -> m (Maybe (Reactor m c))
restore rs (actual,ecs) = new rs <$> second (ctxz actual) <$> foldM k (zero rs) ecs where
	ctxz mrs ns = map pokeNode $ zip mrs ns
	k (dg,ns) (c,e,mrs) = if length mrs /= length ns 
		then error "Restore failed in the numbers of base reactions" 
		else do put c 
			insert' (dg,ctxz mrs ns) e

-- create a reactor from the closure inserting some events
insert :: (Functor m, MonadState c m) => [Rea m] -> (Serialization c,[OpNode m]) -> [External] -> m (Maybe (Reactor m c))
insert rs (dg,ns) es = new rs <$> foldM insert' (dg,ns) es

-- insert one event updating the closure
insert' :: (Functor m, MonadState c m) => (Serialization c,[OpNode m]) -> External -> m (Serialization c,[OpNode m])
insert' (dg,ns) e = do
	c <- get
	let (i,dg') = insertSerialization c e ns dg
	ns' <- catMaybes <$> mapM (react (i,e)) ns 
	return (dg' ,ns') 

