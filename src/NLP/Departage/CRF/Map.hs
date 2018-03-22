{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}


module NLP.Departage.CRF.Map
(
-- * Floats
  Flo (..)

-- * Maps
, Map (..)
, RefMap (..)
, newRefMap

-- * Enumerable maps
, EnumerableMap (..)
) where


import           Control.Monad.Primitive (PrimMonad)
import           Control.Monad.Trans.Class (lift)

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
class Fractional a => Flo a where
  power :: a -> a -> a
  -- | To a double (in normal domain)
  toDouble :: a -> Double

instance Flo Double where
  power = (**)
  toDouble = id

instance Flo F.LogFloat where
  toDouble = F.fromLogFloat
  power x = F.pow x . toDouble


------------------------------------------------------------------
-- Map
------------------------------------------------------------------


-- | A class-based representation of maps from keys to floating-point numbers,
-- so we can plug different implementations (regular maps, hash-based maps,
-- arrays, etc.).
class (PrimMonad prim, Flo v) => Map prim map k v where
  {-# MINIMAL modify, mergeWith #-}

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

--   mergeWith
--     :: (v -> v -> v)
--     -> map k v
--     -> map k v
--     -> prim ()

  -- | Set all elements in the map to `0`
  clear :: Num v => map k v -> prim ()
  clear = modify $ const 0


-- | An extention of `Map` which allows to enumerate the keys in the map.
class Map prim map k v => EnumerableMap prim map k v where
  -- | The stream of keys in the map
  toList :: map k v -> Pipes.ListT prim (k, v)


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
instance (PrimMonad prim, Ord k, Flo v) =>
  Map prim (RefMap prim) k v where

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

--   mergeWith f ref1 ref2 = do
--     m1 <- Ref.readPrimRef $ unRefMap ref1
--     m2 <- Ref.readPrimRef $ unRefMap ref2
--     Ref.writePrimRef (unRefMap ref1) (M.mergeWith f m1 m2)

  clear RefMap{..} = Ref.writePrimRef unRefMap M.empty


instance (Map prim (RefMap prim) k v) =>
  EnumerableMap prim (RefMap prim) k v where

  toList RefMap{..} = do
    m <- lift $ Ref.readPrimRef unRefMap
    Pipes.Select . Pipes.each $ M.toList m
