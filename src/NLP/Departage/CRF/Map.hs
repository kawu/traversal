{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}
-- {-# LANGUAGE TypeFamilies #-}


module NLP.Departage.CRF.Map
(
-- * Floats
  Flo (..)

-- * Maps
, Map (..)
, RefMap (..)
, newRefMap

-- -- * Enumerable maps
-- , EnumerableMap (..)
) where


import           Control.Monad.Primitive (PrimMonad)
import           Control.Monad.Trans.Class (lift)
import qualified Control.Monad.State.Strict as State

import qualified Data.Map.Strict as M
import qualified Data.Map.Merge.Strict as MM

import qualified Data.Number.LogFloat as F
import qualified Data.PrimRef as Ref
import qualified Pipes as Pipes


------------------------------------------------------------------
-- Flo(ating)
------------------------------------------------------------------


-- | A class representing floating point numbers, limited to operations which we
-- rely on.
class (Ord a, Fractional a) => Flo a where
  power :: a -> a -> a
  -- | To a double (in normal domain)
  toDouble :: a -> Double
  -- | To a double (in log domain)
  toLogDouble :: a -> Double

instance Flo Double where
  power = (**)
  toDouble = id
  toLogDouble = log

instance Flo F.LogFloat where
  power x = F.pow x . toDouble
  toDouble = F.fromLogFloat
  toLogDouble = F.logFromLogFloat


------------------------------------------------------------------
-- Domain
------------------------------------------------------------------


-- -- | A function domain descriptor.
-- data Dom k
--   = Set (S.Set k)
--     -- ^ All keys are explicitely enumerated
--   | Range (k,  k)
--     -- ^ Only the min (`fst`) and the max keys are explicitely represented.
--     -- All the keys in between are also a part of the domain.


------------------------------------------------------------------
-- Map Classes
------------------------------------------------------------------


-- | A class-based representation of maps from keys to floating-point numbers,
-- so we can plug different implementations (regular maps, hash-based maps,
-- arrays, etc.).
class (Pipes.MonadIO prim, PrimMonad prim, Ord k, Flo v) => Map prim map k v where
  {-# MINIMAL modify, mergeWith, toList #-}

--   -- | Domain descriptor, which allows to create new maps
--   type Dom k :: *

--   -- | Create a new map whose domain is span between the given
--   new :: Dom k -> prim (map k v)

  -- | Apply the function to all elements in the map
  modify :: (v -> v) -> map k v -> prim ()

  -- | Merge the first map with the second map and store the result in the first
  -- one. Elements in the second map which are not in the first map are ignored.
  mergeWith
    :: Map prim map k w
    => (v -> w -> v)
    -> map k v
    -> map k w
    -> prim ()

  -- | Stream (or pipe) of pairs in the map
  toList :: map k v -> Pipes.ListT prim (k, v)

  -- | Set all elements in the map to `0`
  --
  -- By default implemented as `modify $ const 0`
  clear :: map k v -> prim ()
  clear = modify $ const 0

  -- | Transform the map into a pure key -> value function.
  --
  -- The default implementation relies on `toList` and uses an intermediate
  -- `Data.Map`, so it's not particularly efficient.
  freeze :: map k v -> prim (k -> Maybe v)
  freeze m = do
    regMap <- flip State.runStateT M.empty $ do
      Pipes.runListT $ do
        -- now some hoisting magic...
        (para, val) <- Pipes.hoist lift $ toList m
        lift . State.modify' $ M.insert para val
    return $ \x -> M.lookup x (snd regMap)

  -- | Freeze the map as a pure key -> value function. The function is unsafe in
  -- the sense that it doesn't guarantee that the result `k -> v` is functional
  -- if the input changes while `k -> v` is still in use.
  --
  -- By default `unsafeFreeze = freeze`.
  unsafeFreeze :: map k v -> prim (k -> Maybe v)
  unsafeFreeze = freeze



-- -- | An extention of `Map` which allows to enumerate the keys in the map.
-- class Map prim map k v => EnumerableMap prim map k v where


------------------------------------------------------------------
-- Instances
------------------------------------------------------------------


-- | Referentece to a `M.Map`. We rely on a convention that the value assigned
-- to the elements not in the map is 0.
newtype RefMap prim k v =
  RefMap {unRefMap :: Ref.PrimRef prim (M.Map k v)}


-- | Create a `RefMap` with a given map.
newRefMap :: PrimMonad m => M.Map k v -> m (RefMap m k v)
newRefMap m = do
  ref <- Ref.newPrimRef m
  return $ RefMap ref


-- We require that `Num v` for the sake of `mergeWith`: if an element is not
-- present in the map, than its value is `0`.
instance (Pipes.MonadIO prim, PrimMonad prim, Ord k, Flo v) =>
  Map prim (RefMap prim) k v where

--   -- We don't need to know nothing about the domain to create a new map
--   type Dom k = ()

--   new () = newRefMap M.empty

  modify f RefMap{..} = do
    m <- Ref.readPrimRef unRefMap
    Ref.writePrimRef unRefMap (fmap f m)

  mergeWith f ref1 ref2 = do
    m1 <- Ref.readPrimRef $ unRefMap ref1
    m2 <- Ref.readPrimRef $ unRefMap ref2
    let merged = MM.merge
          MM.preserveMissing -- Preserve element if not in the second map
          (MM.mapMissing $ const (f 0))
          (MM.zipWithMatched $ const f)
          m1 m2
    Ref.writePrimRef (unRefMap ref1) merged

  clear RefMap{..} = Ref.writePrimRef unRefMap M.empty

  toList RefMap{..} = do
    m <- lift $ Ref.readPrimRef unRefMap
    Pipes.Select . Pipes.each $ M.toList m

  freeze RefMap{..} = do
    m <- Ref.readPrimRef unRefMap
    return $ \x -> M.lookup x m
