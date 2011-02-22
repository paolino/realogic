{-# LANGUAGE DeriveDataTypeable, NoMonomorphismRestriction #-}

import Data.Typeable
import Data.Maybe 
import Control.Monad.State
import System.Console.Haskeline

import Data.Reactor.Untypeds
import Data.Reactor.Reaction (Stepper (..),Reaction (..), Response (..))
import Data.Reactor 
import Data.Reactor.Serialization

data Parola = Parola String | Clean deriving (Show,Read,Typeable)
data ParolaS = ParolaS String  deriving (Show,Read,Typeable)

data Numero = Numero Int deriving (Show,Read,Typeable)
data NumeroS = NumeroS Int deriving (Show,Read,Typeable)


r1 :: Reaction (StateT [String] (InputT IO))
r1 = Reaction [Stepper f] (ParolaS "") where
	f (Parola "?") = do 
		ParolaS x <- get
		let f2 (Numero n) = do 
			if n == length x then do 
				lift . lift $ outputStrLn $ "-- bravo!" ++ x
				return $ Response False [] [Untyped Clean]	
				else return $ Response True [] []
		return $ Response True [Reaction  [Stepper f2] ()] []
	f Clean = do
		lift (get >>= lift . outputStrLn . show)
		lift (put [])
		return $ Response True [] []

	f (Parola ".") = return $ Response False [] []
	f (Parola y) = do
		ParolaS x <- get
		lift $ modify (++ if x == y then [x,"repetitae iuvant"] else [y,"riprova"])
		put (ParolaS y)
		return $ Response True [] []


psS = parseSerial [Serial $ ParolaS undefined, Serial ()]
psE = parseSerial [Serial $ Parola undefined, Serial $ Numero undefined]

testr1 = let 
	r =  mkReactor [r1] :: Reactor (StateT [String] (InputT IO)) [String]
	in runInputT defaultSettings . flip runStateT [] $ let 
		k r@(Reactor _ ft t) = do
			lift $ outputStrLn . show $ t
			lift $ outputStrLn . show . fmap (`asTypeOf` t) . parseSerialReactor psS . read . show $ t
			mn <- lift $ getInputLine "%"
			case mn of
				Nothing -> ft t >>= k . fromJust
				Just n -> do 
					let z = psE $ Serial n 
					case z of 
						Nothing -> return ()
						Just ext -> do
							mr' <- insertExternals r [ext] 
							case mr' of
								Nothing -> return ()
								Just r' -> k r'
		in k r
