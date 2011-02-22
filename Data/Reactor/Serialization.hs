
module Data.Reactor.Serialization ( parseSerialReactor, SerialReactor, Serialization) where

import Control.Monad (forM)

import Data.Reactor.Untypeds (ParseSerial, Serial)
import Data.Reactor.MinimalGraph (MinimalGraph (..))


type ReactionStates = [[Maybe Serial]]

type SerialExternal c = (c,Serial,[[Maybe Serial]])

-- | active serialization of a reactor
type Serialization c = MinimalGraph (SerialExternal c) 

-- | passive serialization of a reactor
type SerialReactor c = (ReactionStates,[SerialExternal c])

parseReactionStates :: ParseSerial ReactionStates
parseReactionStates sms mrs = forM mrs $ \rs -> forM rs $ maybe (Just Nothing) (fmap Just . sms ) 


parseSerialExternal :: ParseSerial (SerialExternal c)
parseSerialExternal sms (c,e,mrs) = do
	e' <- sms  e
	mrs' <- parseReactionStates sms mrs
	return (c,e',mrs')

-- | a SerialReactor parser. It tries to fix the existentials, in the Recover and External boxes. Possible templates must be given for the task.
parseSerialReactor :: ParseSerial (SerialReactor c)
parseSerialReactor sms (mrs,ss) = do
	mrs' <- parseReactionStates sms mrs
	ss' <- mapM (parseSerialExternal sms) ss
	return (mrs',ss')




