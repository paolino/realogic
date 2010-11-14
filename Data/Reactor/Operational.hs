-- | Operational values wrap reactions with some historical tag. Function related to Operationals are here
module Data.Reactor.Operational  (Index,Operational (..), poke, expand, context, OpMonad)
	where

import Data.Typeable (cast)
import Control.Monad.Trans (lift) 
import Control.Monad.Writer (WriterT, tell) 
import Control.Monad.Reader (ReaderT, ask)

import Data.Reactor.Untypeds (Serial (Serial))
import Data.Reactor.Reaction (Rea (..), stepRea, Internal, Recover)

-- | The type used later by DepGraph to index external events
type Index = Int

-- | The value stored in the tree nodes, the reaction contextualized with its history
data Operational m = Operational {
	-- | Nothing if the rea was of the boot set, an index to the contextualized event which borned it
	borned :: Maybe Index,
	-- | The reaction if alive or an index to the last event reacted to
	alive :: Either Index (Rea m)
	} 
	
-- | Restore an Operational with a Serialized reaction state
poke 		:: Maybe Serial 	-- ^ the serialized state
		-> Operational m 	-- ^ the operational to restore
		-> Operational m	-- ^ the restored operational

poke (Just (Serial x)) (Operational bnd (Right (Rea f _))) = case cast x of
	Nothing -> error "Restoring type corrupt"
	Just b' -> Operational bnd (Right (Rea f b'))
poke (Just _) _ = error "Restoring a state to to dead reaction"
poke Nothing (Operational _ (Right (Rea f _))) = error "No state to restore an operational"
poke Nothing c = c

-- | The operational transformer which holds the index of the culprit external event in the reader and store the new events in the writer
type OpMonad m = ReaderT Index (WriterT [Internal] m)
-- | Operational expansion , wrap a reaction to produce operationals from operational
expand ::  (Functor m , Monad m) 
	=> Internal -- ^ the event to try 			
	-> Operational m  -- ^ the operational holding a reaction	
	-> OpMonad m (Operational m, [Operational m]) -- ^ return this operational after the possible reaction and a list of new operational wrapping the new reactions
expand _ c@(Operational _ (Left _)) = return (c,[])
expand e (Operational borned (Right r)) = do 
	i <- ask
	(ad,xs') <- case stepRea r e of
		Nothing -> return $ (Right r,[])
		Just k -> lift $ do 
			(xs,es,mrea) <- lift k
			tell es
			return (maybe (Left i) Right mrea, xs)
	return (Operational borned ad,map (Operational (Just i) . Right) xs')
	
-- | compute the historical context of the reaction from the operational
context 	
		:: Operational m  -- ^ the operational
		-> (Maybe Recover,[Index]) -- ^ the actual state of the reaction, if one and 0,1 or 2 indices to external events that must be reacted to create this reaction and bring it to the actual state.
context (Operational (Just i) (Right (Rea _ v))) = (Just (Serial v),[i])
context	(Operational (Just i) (Left j)) = (Nothing, [i,j])
context	(Operational Nothing (Left j)) = (Nothing, [j])
context (Operational Nothing (Right (Rea _ v))) = (Just (Serial v), [])

