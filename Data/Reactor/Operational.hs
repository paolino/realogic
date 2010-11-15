{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, NoMonomorphismRestriction, MultiParamTypeClasses #-}
-- | Operational values wrap reactions with some historical tag. Index is taken from "Data.Reactor.MinimalGraph" and is a key to an happened External event.
module Data.Reactor.Operational  (Operational (..), OperationalLayer, mkOperationalPruned)
	where

import Data.Typeable (cast)
import Control.Monad.Trans (lift) 
import Control.Monad.Writer (WriterT, tell) 
import Control.Monad.Reader (ReaderT, ask)

import Data.Reactor.Untypeds (Serial (Serial))
import Data.Reactor.Reaction (Reaction (..), step, Internal, Recover)
import Data.Reactor.MinimalGraph (Index)
import Data.Reactor.Pruned (mkPruned, Pruned)


-- | 'Reaction' contextualized with its history.
data Operational m = Operational {
	-- | Nothing if the reaction is a base reaction  or just an index to the contextualized event which borned it
	borned :: Maybe Index,
	-- |  an index to the event that killed the reaction or the alive reaction
	alive :: Either Index (Reaction m)
	} 
	

-- | The operational transformer. For reactions to react we store the index to external event and the actual internal event in the reader and store the new events produced by reactions in the writer
type OperationalLayer m = ReaderT (Index,Internal) (WriterT [Internal] m)

-- | Pruned object builder for 'Operational' values 
mkOperationalPruned :: (Functor m, Monad m) 
	=> Operational m		-- ^ the Operational for the base
        -> Pruned (OperationalLayer m) (Maybe Serial, [Index]) -- ^ the fresh pruned object 

mkOperationalPruned = mkPruned opexpand opprune oprestore opserialize where
	opexpand :: (Functor m, Monad m) => Operational m -> OperationalLayer m (Operational m, [Operational m])
	opexpand c@(Operational _ (Left _)) = return (c,[]) 
	opexpand (Operational borned (Right r)) = do 
		(i,e) <- ask
		(ad,xs') <- case step r e of
			Nothing -> return $ (Right r,[])
			Just k -> lift $ do 
				(xs,es,mrea) <- lift k
				tell es
				return (maybe (Left i) Right mrea, xs)
		return (Operational borned ad,map (Operational (Just i) . Right) xs')

	opserialize (Operational (Just i) (Right (Reaction _ v))) = (Just (Serial v),[i])
	opserialize (Operational (Just i) (Left j)) = (Nothing, [i,j])
	opserialize (Operational Nothing (Left j)) = (Nothing, [j])
	opserialize (Operational Nothing (Right (Reaction _ v))) = (Just (Serial v), [])

	oprestore (Operational bnd (Right (Reaction f _))) (Just (Serial x),_)  = case cast x of
		Nothing -> error "Restoring type corrupt"
		Just b' -> Operational bnd (Right (Reaction f b'))
	oprestore _ (Just _,_)  = error "Restoring a state to to dead reaction"
	oprestore  (Operational _ (Right (Reaction f _))) (Nothing,_) = error "No state to restore an operational"
	oprestore c (Nothing,_)  = c

	opprune (Operational _ (Left _)) = True
	opprune  _ = False

