-- | Smart constructor for representing a probability map over discrete items.
-- We do not require that the sum of probabilities = 1, the map can correspond
-- to arcs with 0 probability mass.


module NLP.Traversal.Prob
  ( Prob
  , fromList
  , unProb
  , toList
  ) where


-- import           Control.Arrow (second)

import qualified Data.Map.Strict as M


-- | A probability distribution over items of type `a`.
newtype Prob a = Prob {unProb :: M.Map a Double}
  deriving (Show, Eq, Ord)


-- | Probability smart constructor.
fromList :: (Ord a) => [(a, Double)] -> Prob a
fromList
  = Prob
  . M.fromListWith (+)
  . filter ((>=0) . snd)


-- | List representation.
toList :: Prob a -> [(a, Double)]
toList = M.toList . unProb
