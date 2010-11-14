module Data.Reactor.OpNode where

import Prelude hiding (foldr, concat)
import Data.List (nub)
import Data.Traversable (mapAccumL) 
import Data.Foldable (foldr, concat)
import Control.Monad (foldM) 
import Control.Monad.Writer (WriterT, runWriterT, listen) 
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Applicative ((<$>))

import Data.Reactor.Untypeds (Serial , toUntyped)
import Data.Reactor.Node (prune,expandM,Node)
import Data.Reactor.Reaction (Internal, External, Recover)

import Data.Reactor.Operational (Operational (..), expand,Index, context, poke)


type OpNode m = Node (Operational m)

pruneReact ::	(Functor m, Monad m) 
	=> Internal
	-> OpNode m
	-> ReaderT Index (WriterT [Internal] m) (Maybe (OpNode m))
pruneReact e n = prune isDead <$> expandM (expand e) n where
	isDead (Operational _ (Left _)) = True
	isDead _ = False

react :: (Monad m, Functor m) =>   (Index,External) -> OpNode m -> m (Maybe (OpNode m))
react (i,e) n = fmap fst . runWriterT . flip runReaderT i . z (Just n) $ toUntyped e where
	z Nothing _ = return Nothing
	z (Just n') e' = do
		(n'',es) <- listen $ pruneReact e' n'
		if null es then return n'' else case n'' of
			Nothing -> return Nothing -- information leak
			n''' -> foldM z n''' es

contextualize :: OpNode m -> ([Maybe Recover],[Index])
contextualize n = let (rs,is) = unzip . map context $ foldr (:) [] n in (rs,nub . concat $ is)

pokeNode :: ([Maybe Serial], OpNode m) -> OpNode m
pokeNode (mr,n) = snd . mapAccumL poke' mr $ n where
	poke' [] _ = error "Not enough stores states to restore the reaction tree"
	poke' (x:xs) o = (xs, poke x o)

