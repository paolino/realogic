{-# LANGUAGE ScopedTypeVariables, ExistentialQuantification, UnicodeSyntax #-}

module Untypeds  where

import Data.Typeable (Typeable, cast)
import Data.Maybe (mapMaybe, listToMaybe)
import Control.Applicative ((<$>))
import Control.Arrow (first)
import Control.Monad (join)
import Text.Read hiding ((<++))
import Text.ParserCombinators.ReadP


-- | A box to contain any value 
data Untyped =  ∀ b. (Typeable b) ⇒ Untyped b

casting :: (Typeable a, Typeable c) => (a -> b) -> c -> Maybe b
casting f x = f <$> cast x

-- | A box to contain a serializable 
data Serial = ∀ b. (Read b, Show b,Typeable b) ⇒ Serial b

instance Show Serial where
	show (Serial x) = "{" ++ show x ++ "}"

instance Read Serial where
	readPrec = fmap Serial . lift $ do
		skipSpaces
		char '{'
		s <- 	do
				(s::String) <- readS_to_P  reads
				return (show s)
			<++ munch (/= '}')
		char '}'
		return s

-- | Try to parse a string into a Serial box. It needs some hints on which types could go inside the box
parseSerial ss y  = fmap fst . listToMaybe . mapMaybe (parseSerial' y) $ ss where
	parseSerial' (Serial y) (Serial x) = join $ 
		listToMaybe <$> map (first (Serial . (`asTypeOf` x))) <$> reads <$> cast y 

type ParseSerial a = (Serial -> Maybe Serial) -> a  -> Maybe a
