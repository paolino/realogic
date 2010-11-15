
module Data.Reactor.Serialization (parseSerialReactor, SerialReactor, Serialization) where

import Data.List (nub)
import Control.Monad (forM)

import Data.Reactor.Untypeds (ParseSerial)
import Data.Reactor.MinimalGraph (MinimalGraph (..))
import Data.Reactor.Reaction (External, Recover)
import Data.Reactor.Operational -- (Index)

type ReactionStates = [[Maybe Recover]]

type SerialExternal c = (c,External,[[Maybe Recover]])

-- | active serialization of a reactor
type Serialization c = MinimalGraph (SerialExternal c) 

-- | passive serialization of a reactor
type SerialReactor c = (ReactionStates,[SerialExternal c])

parseReactionStates :: ParseSerial ReactionStates
parseReactionStates ps mrs = forM mrs $ \rs -> forM rs $ maybe (Just Nothing) (fmap Just . ps)


parseSerialExternal :: ParseSerial (SerialExternal c)
parseSerialExternal ps (c,e,mrs) = do
	e' <- ps e
	mrs' <- parseReactionStates ps mrs
	return (c,e',mrs')

-- | a SerialReactor parser. It tries to fix the existentials, in the Recover and External boxes. Possible templates must be given for the task.
parseSerialReactor :: ParseSerial (SerialReactor c)
parseSerialReactor ps (mrs,ss) = do
	mrs' <- parseReactionStates ps mrs
	ss' <- mapM (parseSerialExternal ps) ss
	return (mrs',ss')





