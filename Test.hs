import Test.QuickCheck
import Data.Maybe
import Data.List
import Data.Foldable (toList)
import Control.Monad.State
import Control.Monad.Identity
import Data.Typeable
import Control.Applicative ((<$>))

import Data.Reactor.MinimalGraph (prop_data_reactor_minimalgraph) 
import Data.Reactor.Pruned (prop_data_reactor_pruned)
import Data.Reactor.Reaction (prop_data_reactor_reaction) 


main = mapM_ (quickCheckWith (Args {replay = Nothing, maxSuccess = 200, maxDiscard = 500, maxSize = 100})) 
	[	prop_data_reactor_reaction,
		prop_data_reactor_pruned,
		prop_data_reactor_minimalgraph	
	]
