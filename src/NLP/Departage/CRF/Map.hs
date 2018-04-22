{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}


module NLP.Departage.CRF.Map
(
-- * Floats
  Flo (..)

-- * Maps
, Map (..)
, Encoding (..)
, save
, load
-- ** RefMap
, RefMap (..)
-- ** IndiMap
, IndiMap (..)

-- -- * Enumerable maps
-- , EnumerableMap (..)
) where


import           Control.Monad (forM_, unless)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Primitive (PrimMonad, PrimState)
import           Control.Monad.Trans.Class (lift)
import qualified Control.Monad.State.Strict as State

-- import           Data.Vector.Generic.Base (Vector)
-- import           Data.Vector.Generic.Mutable (MVector)
import           Data.Vector.Unboxed.Deriving -- (derivingUnbox)
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM

import           Data.Ord (comparing)
import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import qualified Data.Map.Merge.Strict as MM
import qualified Data.HashMap.Strict as HM
import           Data.Hashable (Hashable)

-- import           Data.Number.LogFloat (LogFloat(..))
import qualified Data.Number.LogFloat as F
import qualified Data.PrimRef as Ref
import qualified Pipes as Pipes


------------------------------------------------------------------
-- Flo(ating)
------------------------------------------------------------------


-- | A class representing floating point numbers, limited to operations which we
-- rely on.
class (Ord a, Fractional a, UM.Unbox a) => Flo a where
  power :: a -> a -> a
  -- | To a double (in normal domain)
  toDouble :: a -> Double
  -- | To a double (in log domain)
  toLogDouble :: a -> Double
  -- | From a double (in normal domain)
  fromDouble :: Double -> a

instance Flo Double where
  power = (**)
  toDouble = id
  toLogDouble = log
  fromDouble = id

instance Flo F.LogFloat where
  power x = F.pow x . toDouble
  toDouble = F.fromLogFloat
  toLogDouble = F.logFromLogFloat
  fromDouble = F.logFloat

-- deriving instance (Vector UM.IOVector) Flo

derivingUnbox "LogFloat"
  [t| F.LogFloat -> Double |]
  [| F.fromLogFloat |]
  [| F.logFloat |]


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
  {-# MINIMAL fromMap, modify, mergeWith, mergeWithList, toList #-}

--   -- | Domain descriptor, which allows to create new maps
--   type Dom k :: *

--   -- | Create a new map whose domain is span between the given
--   new :: Dom k -> prim (map k v)

  -- | Create from a pure map.
  fromMap :: M.Map k v -> prim (map k v)

  -- | Transform the map into a pure key -> value map.
  --
  -- The default implementation relies on `toList` and uses an intermediate
  -- `Data.Map`, so it's not particularly efficient.
  --
  -- TODO: Actually, `toList` could be also implemented in terms of `toMap`.
  toMap :: map k v -> prim (M.Map k v)
  toMap m = do
    regMap <- flip State.runStateT M.empty $ do
      Pipes.runListT $ do
        -- now some hoisting magic...
        (para, val) <- Pipes.hoist lift $ toList m
        lift . State.modify' $ M.insert para val
    return (snd regMap)


  -- | Apply the function to all elements in the map
  modify :: (v -> v) -> map k v -> prim ()

  -- | Merge the first map with the second map and store the result in the first
  -- one.
  --
  -- OBSOLETE: Elements in the second map which are not in the first map are ignored.
  --
  -- NEW 22.04.2018: Elements present only in the second map are assumed to have
  -- value 0 in the first map.
  --
  mergeWith
    :: Map prim map k w
    => (v -> w -> v)
    -> map k v
    -> map k w
    -> prim ()

  -- | Merge the first map with another map represented as a list.
  -- See also `mergeWith`.
  mergeWithList
    :: (v -> v -> v)
    -> map k v
    -> [(k, v)]
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
  -- The default implementation is based on `toMap`.
  freeze :: map k v -> prim (k -> Maybe v)
  freeze m = do
    pureMap <- toMap m
    return $ \x -> M.lookup x pureMap

  -- | Freeze the map as a pure key -> value function. The function is unsafe in
  -- the sense that it doesn't guarantee that the result `k -> v` is functional
  -- if the input changes while `k -> v` is still in use.
  --
  -- By default `unsafeFreeze = freeze`.
  unsafeFreeze :: map k v -> prim (k -> Maybe v)
  unsafeFreeze = freeze


-- | Encoding from and to a string.
data Encoding a = Encoding
  { toStr :: a -> String
  , fromStr :: String -> a
  }


-- | Save the map in a file.
save
  :: (Map prim map k v)
  => Encoding k
  -> map k v
  -> FilePath
  -> prim ()
save Encoding{..} m filePath = do
  pureMap <- toMap m
  liftIO . writeFile filePath . unlines $ do
    (key, val) <- reverse . L.sortBy (comparing snd) $ M.toList pureMap
    return $ concat
      [ show $ toDouble val
      , "\t"
      , toStr key ]

-- | Load the map from the file
load
  :: (Map prim map k v)
  => Encoding k
  -> FilePath
  -> prim (map k v)
load Encoding{..} filePath = do
  contents <- liftIO $ readFile filePath
  let m = M.fromList $ do
        line <- lines contents
        let (left, right) = L.break (=='\t') line
        return (fromStr right, fromDouble $ read left)
  fromMap m


------------------------------------------------------------------
-- RefMap Instance
------------------------------------------------------------------


-- | Reference to a `M.Map`. We rely on a convention that the value assigned to
-- the elements not in the map is 0.
newtype RefMap prim k v =
  RefMap {unRefMap :: Ref.PrimRef prim (M.Map k v)}

-- We require that `Num v` for the sake of `mergeWith`: if an element is not
-- present in the map, than its value is `0`.
instance (Pipes.MonadIO prim, PrimMonad prim, Ord k, Flo v) =>
  Map prim (RefMap prim) k v where

--   -- We don't need to know nothing about the domain to create a new map
--   type Dom k = ()

--   new () = newRefMap M.empty

  fromMap m = do
    ref <- Ref.newPrimRef m
    return $ RefMap ref

  modify f RefMap{..} = do
    m <- Ref.readPrimRef unRefMap
    Ref.writePrimRef unRefMap (fmap f m)

  mergeWith f ref1 ref2 = do
    m1 <- Ref.readPrimRef $ unRefMap ref1
    m2 <- Ref.readPrimRef $ unRefMap ref2
    let merged = MM.merge
          -- Preserve elements not in the second map
          MM.preserveMissing
          -- Those which are in the second map but not in the first map are
          -- treated as if they had value 0 in the first map
          (MM.mapMissing $ const (f 0))
          -- Those which are in both maps
          (MM.zipWithMatched $ const f)
          m1 m2
    Ref.writePrimRef (unRefMap ref1) merged

  mergeWithList f ref1 xs = do
    m1 <- Ref.readPrimRef $ unRefMap ref1
    let update m (key, val) = M.insertWith f key val m
        merged = L.foldl' update m1 xs
    Ref.writePrimRef (unRefMap ref1) merged

  clear RefMap{..} = Ref.writePrimRef unRefMap M.empty

  toList RefMap{..} = do
    m <- lift $ Ref.readPrimRef unRefMap
    Pipes.Select . Pipes.each $ M.toList m

  freeze RefMap{..} = do
    m <- Ref.readPrimRef unRefMap
    return $ \x -> M.lookup x m

--   save Encoding{..} RefMap{..} filePath = do
--     m <- Ref.readPrimRef unRefMap
--     liftIO . writeFile filePath . unlines $ do
--       (key, val) <- reverse . L.sortBy (comparing snd) $ M.toList m
--       return $ concat
--         [ show $ toDouble val
--         , "\t"
--         , toStr key ]
--
--   load Encoding{..} filePath = do
--     contents <- liftIO $ readFile filePath
--     let m = M.fromList $ do
--           line <- lines contents
--           let (left, right) = L.break (=='\t') line
--           return (fromStr right, fromDouble $ read left)
--     newRefMap m


------------------------------------------------------------------
-- TODO: Another Instance
------------------------------------------------------------------


-- | An indirect map?
data IndiMap prim k v = IndiMap
  -- { indiSet :: S.Set k
  { indiMap :: HM.HashMap k Int
    -- ^ Domain of the map.
  , indiVect :: UM.MVector (PrimState prim) v
    -- ^ Vector of values. The value on position `i` corresponds to the key with
    -- index `i` in `indiMap`.
  }


-- We require that `Num v` for the sake of `mergeWith`: if an element is not
-- present in the map, than its value is `0`.
instance
  (Hashable k, Pipes.MonadIO prim, PrimMonad prim, Ord k, Flo v)
  => Map prim (IndiMap prim) k v where

--   -- We don't need to know nothing about the domain to create a new map
--   type Dom k = ()

--   new () = newRefMap M.empty

  fromMap m = do
    v <- UM.replicate (M.size m) 0
    let hm = HM.fromList $ zip (M.keys m) [0..]
    forM_ (zip [0..] $ M.elems m) $ \(ix, val) -> do
      UM.unsafeWrite v ix val
    return $ IndiMap
      { indiMap = hm
      , indiVect = v }

  modify f IndiMap{..} = do
    forM_ [0 .. UM.length indiVect - 1] $ \i -> do
      x <- UM.unsafeRead indiVect i
      UM.unsafeWrite indiVect i (f x)

  mergeWith f m1 m2 = do
    let v1 = indiVect m1
        v2 = indiVect m2
    unless (UM.length v1 == UM.length v2) $
      error "CRF.Map.mergeWith: different lengths!"
    forM_ [0 .. UM.length v1 - 1] $ \i -> do
      x <- UM.unsafeRead v1 i
      y <- UM.unsafeRead v2 i
      UM.unsafeWrite v1 i (f x y)

  mergeWithList f IndiMap{..} xs = do
    forM_ xs $ \(key, val) -> do
      -- case S.lookupIndex key indiSet of
      case HM.lookup key indiMap of
        Nothing -> return ()
        Just ix -> do
          x <- UM.unsafeRead indiVect ix
          UM.unsafeWrite indiVect ix (f x val)

  clear IndiMap{..} = UM.set indiVect 0

  toList IndiMap{..} = Pipes.Select $ do
--     let ks = S.toList indiSet
--         is = [0..]
--     forM_ (zip ks is) $ \(key, ix) -> do
    forM_ (HM.toList indiMap) $ \(key, ix) -> do
      val <- lift $ UM.unsafeRead indiVect ix
      Pipes.yield (key, val)

  freeze IndiMap{..} = do
    frozen <- U.freeze indiVect
    return $ \key -> do
      -- ix <- S.lookupIndex key indiSet
      ix <- HM.lookup key indiMap
      return $ frozen U.! ix

  unsafeFreeze IndiMap{..} = do
    frozen <- U.unsafeFreeze indiVect
    return $ \key -> do
      -- ix <- S.lookupIndex key indiSet
      ix <- HM.lookup key indiMap
      return $ frozen U.! ix
