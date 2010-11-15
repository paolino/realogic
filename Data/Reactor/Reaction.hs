{-# LANGUAGE  	ExistentialQuantification, 
		UnicodeSyntax, DeriveFoldable, DeriveFunctor, 
		DeriveTraversable
		#-}
-- | Reaction box and stepping function. 'Reaction's leave the monad parameter free for the programmer. Around m a state transformer gives them the chance to use per reaction state.
module Data.Reactor.Reaction  (Reaction (..) , step, External, Internal, Recover
	, Response (..), prop_data_reactor_reaction) 
	where

import Data.Typeable (Typeable, cast)
import Control.Monad.State (StateT, runStateT) 
import Control.Applicative ((<$>))

import Data.Reactor.Untypeds (Serial , Untyped (Untyped))

import Test.QuickCheck
import Control.Monad.Identity (runIdentity)
import Control.Monad.State (modify, foldM) 

-- | Internal event, don't need to be serializable
type Internal = Untyped

-- | External event, it must be serializable
type External = Serial

-- | Internal state serializations, it must be serializable
type Recover = Serial

-- | The value reactions compute.
data Response m = Response { 
	continue :: Bool ,		-- ^ True to continue the reaction, or False if reaction if dead
	newreas :: [Reaction m] , 	-- ^ a list of new reactions, just borned ready for the next event
	newevents :: [Internal] 	-- ^ some events to broadcast now as effects of the reaction
	}
-- | A Reaction object is a container for a reaction. It's free in the type of value to react and in its internal state.
data Reaction m  = forall a b . (Typeable a, Show b, Read b, Typeable b) => Reaction {
	-- | The reaction to an event of type 'a'. It can modify its individual state in the outer monad layer. There are no constraint on the inner monad.
		reaction :: a -> StateT b m (Response m) 
	-- | Internal state of the reaction. Upon creation it must contatin the initial state.
	,	reastate :: b			
	}


-- | Try a reaction of a Reaction, given an event in an Untyped box. If the event is not of the right type, the result is Nothing, otherwise an action in the monad m returning a modified 'Response', with Bool mapped to Maybe (Reaction m).
step :: (Monad m, Functor m) 
	=> Reaction m 		-- ^ reaction box containing the reaction to try 
	-> Internal 		-- ^ the event for the reaction
	-> Maybe (m 	(
			[Reaction m], 
			[Internal], 
			Maybe (Reaction m) 
			)
		) -- ^ new reactions, events and possibly a renewed Reaction, or Nothing if the reaction is dead

step (Reaction f b) (Untyped x) = k <$> cast x where
	k x' = do
		(Response t xs ys, b') <- runStateT (f x') b
		return (xs,ys, if t then Just $ Reaction f b' else Nothing)



-------------- quick check property --------------------------		
prop_data_reactor_reaction :: Gen Bool

prop_data_reactor_reaction = do
	ms <- listOf (elements [1..10::Int])
	let 	r = Reaction (\y -> modify (+ y) >> return (Response True [] [])) (0::Int) 
		ck Nothing  _ = return Nothing
		ck (Just r') x = do 
				let k = step r' x
				case k of 
					Nothing -> return Nothing
					Just k' -> do
						(_,_,mr) <- k'
						return mr
		ef = foldM ck (Just r) $ map Untyped ms
		q = case runIdentity ef of 
			Just (Reaction _ z) -> case cast z of
				Just z' ->  z' == sum ms
				Nothing -> False
			Nothing -> False
	return q

	

	
