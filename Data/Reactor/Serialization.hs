
module Data.Reactor.Serialization (parseSerialReactor, SerialReactor, Serialization, insertSerialization, purgeSerialization) where

import Data.List (nub)
import Control.Monad (forM)

import Data.Reactor.Untypeds (ParseSerial)
import Data.Reactor.DepGraph (DepGraph (..))
import Data.Reactor.Reaction (External, Recover)
import Data.Reactor.Operational (Index)
import Data.Reactor.OpNode (OpNode, contextualize)

type ReactionStates = [[Maybe Recover]]

type SerialExternal c = (c,External,[[Maybe Recover]])

-- | active serialization of a reactor
type Serialization c = DepGraph (SerialExternal c) 

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

-- | wraps the add DepGraph method for the Serialization object
insertSerialization 	:: c 		-- ^ reactor global state
			-> External 	-- ^ event to happen 
			-> [OpNode m] 	-- ^ reaction trees
			-> Serialization c -- ^ serialization object
			-> (Index,Serialization c) -- ^ updated object

insertSerialization c e xs (DepGraph add' _ _) = let (mrs,iss) = unzip $ map contextualize xs in 
		add' ((c,e,mrs),nub . concat $ iss)

-- | 
purgeSerialization :: [OpNode m] ->  Serialization c -> Maybe (Serialization c)
purgeSerialization ns (DepGraph _ resize' _) = resize' (nub . concat . map (snd . contextualize) $ ns)




