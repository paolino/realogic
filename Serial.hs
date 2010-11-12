{-# LANGUAGE ExistentialQuantification, UnicodeSyntax #-}

module Untypeds (..) where

import Data.Typeable (Typeable)
import Data.Maybe (mapMaybe, listToMaybe)
import Control.Applicative ((<$>))


-- | A box to contain any value 
data Untyped =  ∀ b. (Typeable b) ⇒ Untyped b

casting :: (Typeable a, Typeable c) => (a -> b) -> c -> Maybe b
casting f x = f <$> cast x

-- | A box to contain a serializable 
data Serial = ∀ b. (Read b, Show b,Typeable b) ⇒ Serial b

instance Show Serial where
	show (Serial x) = show (show x)

-- | Try to parse a string into a Serial box. It needs some hints on which types could go inside the box
parseSerial ∷ String → [Serial] → Maybe Serial
parseSerial y  = listToMaybe . mapMaybe (parseSerial' y) where
	parseSerial' y (Serial x) = case reads y of
		[] → Nothing
		[(y',_)] → Serial . (`asTypeOf` x) <$> y' 

