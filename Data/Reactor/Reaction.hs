{-# LANGUAGE  	ExistentialQuantification, 
		UnicodeSyntax, DeriveFoldable, DeriveFunctor, 
		DeriveTraversable, TupleSections,
		MultiParamTypeClasses
		#-}

module Data.Reactor.Reaction  (Rea (..) , stepRea, External, Internal, Recover, Response (..)) 
	where

import Data.Typeable (Typeable, cast)
import Control.Monad.State (StateT, runStateT) 
import Control.Applicative ((<$>))

import Data.Reactor.Untypeds (Serial , Untyped (Untyped))


-- | Internal events are untyped, and not necessarily serializable
type Internal = Untyped

-- | external events are untyped, and must be serializable
type External = Serial

-- | internal state serializations for reactions are untyped
type Recover = Serial

-- | The value Rea objects must return.
data Response m = Response { 
	continue :: Bool ,			-- ^ True to continue the reaction, or False if reaction if dead
	newreas :: [Rea m] , 		-- ^ a list of new reactions, just borned ready for the next event
	newevents :: [Internal] 		-- ^ some events to broadcast now as effects of the reaction
	}
-- | Rea is the main interface to this module. It's exixtentially typed in the type of value to react and in its internal state.
data Rea m  = forall a b . (Typeable a, Show b, Read b, Typeable b) => Rea {
	-- | the next reaction to an event of type 'a'. The reaction can modify its individual state in the outer 
	-- monad layer. There is no constraint on the inner monad.
		reaCtion :: a -> StateT b m (Response m) 
	-- | the internal state of the reaction , everything closed
	,	reaState :: b			
	}


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
		
	
	

	
