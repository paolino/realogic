{-# LANGUAGE  	ExistentialQuantification#-}

-- | Reaction box and stepping function. 'Reaction's leave the monad parameter free for the programmer. Around m a state transformer gives them the chance to use per reaction state.
module Data.Reactor.Reaction  	where

import Data.Typeable (Typeable, cast)
import Control.Monad.State (StateT, runStateT) 
import Control.Applicative ((<$>))

import Data.Reactor.Untypeds (Serial , Untyped (Untyped))

import Test.QuickCheck
import Control.Monad.Identity (runIdentity)
import Control.Monad.State (modify, foldM) 



data Stepper m b = forall a. Typeable a => Stepper (a -> StateT b m (Response m))

-- | Internal event, don't need to be serializable
type Event = Untyped
-- | The value reactions compute.
data Response m = Response { 
	continue :: Bool ,		-- ^ True to continue the reaction, or False if reaction if dead
	born :: [Reaction m] , 	-- ^ a list of new reactions, just borned ready for the next event
	broadcast :: [Event] 	-- ^ some events to broadcast now as effects of the reaction
	}

-- | A Reaction object is a container for a reaction. It's free in the type of value to react and in its internal state.
data Reaction m  = forall b . (Show b, Read b, Typeable b) => Reaction {
	-- | The reaction to an event of type 'a'. It can modify its individual state in the outer monad layer. There are no constraint on the inner monad.
		reactors :: [Stepper m b]
	-- | Internal state of the reaction. Upon creation it must contain the initial state.
	,	state :: b			
	}

type Effect m b = ([Reaction m], [Event], Maybe b)

-- | Try a reaction of a Reaction, given an event in an Untyped box. If the event is not of the right type, the result is Nothing, otherwise an action in the monad m returning a modified 'Response', with Bool mapped to Maybe (Reaction m).
stepStep :: ( Typeable a, Monad m, Functor m) 
	=> (a -> StateT b m (Response m))		-- ^ reaction box containing the reaction to try 
	-> b
	-> Event
	-> Maybe (m (Effect m b))
stepStep f b (Untyped x) = cast x >>= \x' -> return $ do
		(Response t xs ys, b') <- runStateT (f x') b
		return (xs,ys, if t then Just b' else Nothing)
stepStepper :: (Monad m, Functor m) 
	=> Stepper m b 		-- ^ reaction box containing the reaction to try 
	-> b
	-> Event
	-> Maybe (m (Effect m b))
stepStepper (Stepper s) b x = stepStep s b x

fStepSteppers :: (Monad m , Functor m) => Event -> Effect m b -> Stepper m b -> m (Effect m b)
fStepSteppers x (rs,is,Nothing) s = return (rs,is,Nothing)
fStepSteppers x (rs,is,Just b) s = case stepStepper s b x of
	Nothing ->  return (rs,is,Nothing)
	Just f -> do
		(rs',is',mb') <- f
		return (rs ++ rs', is ++ is', mb')

step ::  (Monad m , Functor m) => Event -> Reaction m -> m ([Reaction m], [Event] , Maybe (Reaction m))
step x (Reaction reas st) = do 
	(rs,is,mb) <- foldM (fStepSteppers x) ([],[],Just st) reas
	return (rs,is, Reaction reas <$> mb)



-------------- quick check property --------------------------		
prop_data_reactor_reaction :: Gen Bool

prop_data_reactor_reaction = do
	ms <- listOf (elements [1..10::Int])
	let 	r = Reaction [Stepper (\y -> modify (+ y) >> return (Response True [] []))] (0::Int) 
		ck Nothing  _ = return Nothing
		ck (Just r') x = do 
				(_,_,mr) <- step x r'
				return mr
		ef = foldM ck (Just r) $ map Untyped ms
		q = case runIdentity ef of 
			Just (Reaction _ z) -> case cast z of
				Just z' ->  z' == sum ms
				Nothing -> False
			Nothing -> False
	return q
	

	
