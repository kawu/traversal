{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}


-- | Stochastic gradient descent for hyper CRFs.


module NLP.Departage.CRF.SGD
( SgdArgs(..)
, sgdArgsDefault
, sgd
) where


-- import qualified Control.Monad.Primitive as Prim
import           Control.Monad.Primitive (PrimMonad)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Trans.Class (lift)

import qualified System.Random as R

-- import qualified Data.Vector.Unboxed as U
-- import qualified Data.Vector.Unboxed.Mutable as UM

import qualified NLP.Departage.CRF.Map as Map
import           NLP.Departage.CRF.Map (Map)
import qualified NLP.Departage.CRF.Mame as Mame
import           NLP.Departage.CRF.Mame (Mame)
import qualified NLP.Departage.CRF.SGD.Dataset as D


------------------------------------------------------------------
-- Params
------------------------------------------------------------------


 -- | SGD parameters controlling the learning process.
data SgdArgs = SgdArgs
    { -- | Size of the batch
      batchSize :: Int
    -- | Regularization variance
    , regVar    :: Double
    -- | Number of iterations
    , iterNum   :: Double
    -- | Initial gain parameter
    , gain0     :: Double
    -- | After how many iterations over the entire dataset
    -- the gain parameter is halved
    , tau       :: Double
    -- | The gamma parameter which drives momentum; TODO: explain
    , gamma     :: Double
    }


-- | Default SGD parameter values.
sgdArgsDefault :: SgdArgs
sgdArgsDefault = SgdArgs
    { batchSize = 50
    , regVar    = 10
    , iterNum   = 10
    , gain0     = 0.25
      -- ^ Without momentum I would rather go for '1', but the gradient with
      -- momentum becomes much larger.
    , tau       = 5
    , gamma     = 0.9
    }


-- ------------------------------------------------------------------
-- -- Types
-- ------------------------------------------------------------------
--
--
-- -- | Vector of parameters.
-- type Para       = U.Vector Double
--
--
-- -- | Type synonym for mutable vector with Double values.
-- type MVect      = UM.MVector (Prim.PrimState IO) Double


------------------------------------------------------------------
-- Momentum
------------------------------------------------------------------


-- -- | Stochastic gradient descent. A notification function can be used to provide
-- -- user with information about the progress of learning.
-- sgd
--   :: SgdArgs                  -- ^ SGD parameters (config)
--   -> (Para -> Int -> IO ())   -- ^ Notification run every update
--   -> (Para -> x -> Grad)      -- ^ Gradient for dataset element
--   -> Dataset x                -- ^ Dataset
--   -> Para                     -- ^ Starting point
--   -> IO Para                  -- ^ SGD result
-- sgd SgdArgs{..} notify mkGrad dataset x0 = do
--
--   putStrLn $ "Running momentum!"
--
--   -- A vector for the momentum gradient
--   momentum <- UM.new (U.length x0)
--
--   -- A worker vector for computing the actual gradients
--   u <- UM.new (U.length x0)
--
--   doIt momentum u 0 (R.mkStdGen 0) =<< U.thaw x0
--
--   where
--     -- Gain in k-th iteration.
--     gain k = (gain0 * tau) / (tau + done k)
--
--     -- Number of completed iterations over the full dataset.
--     done :: Int -> Double
--     done k
--         = fromIntegral (k * batchSize)
--         / fromIntegral (size dataset)
--     doneTotal :: Int -> Int
--     doneTotal = floor . done
--
--     -- Regularization (Guassian prior) parameter
--     regularizationParam = regCoef
--       where
--         regCoef = iVar ** coef
--         iVar = 1.0 / regVar
--         coef = fromIntegral (size dataset)
--              / fromIntegral batchSize
--
--     doIt momentum u k stdGen x
--
--       | done k > iterNum = do
--         frozen <- U.unsafeFreeze x
--         notify frozen k
--         return frozen
--
--       | otherwise = do
--
--         -- Sample the dataset
--         (batch, stdGen') <- sample stdGen batchSize dataset
--
--         -- NEW: comment out
--         -- -- Apply regularization to the parameters vector.
--         -- scale (regularization k) x
--
--         -- Freeze mutable vector of parameters. The frozen version is
--         -- then supplied to external mkGrad function provided by user.
--         frozen <- U.unsafeFreeze x
--         notify frozen k
--
--         -- Compute the gradient and put it in `u`
--         let grad = parUnions (map (mkGrad frozen) batch)
--         addUp grad u
--
--         -- Apply regularization to `u`
--         applyRegularization regularizationParam x u
--
--         -- Scale the gradient
--         scale (gain k) u
--
--         -- Compute the new momentum
--         updateMomentum gamma momentum u
--
--         x' <- U.unsafeThaw frozen
--         momentum `addTo` x'
--         doIt momentum u (k+1) stdGen' x'


-- | Stochastic gradient descent. A notification function can be used to provide
-- user with information about the progress of learning. The computation is
-- performed within the `Mame` monad, which provides new vectors of the same
-- size as the parameters vector on demand.
sgd
  :: (Map IO map k Double, MonadIO m)
  => SgdArgs
  -- ^ SGD parameters (config)
  -> (Int -> m ())
  -- ^ Notification run every update
  -> (map k Double -> [x] -> m ())
  -- ^ Gradient for dataset elements; we allow it to be in its own monad
  -> D.Dataset x
  -- ^ Dataset
  -> map k Double
  -- ^ Starting point
  -- -> IO (map k Double)        -- ^ SGD result
  -> Mame (map k Double) m ()
sgd SgdArgs{..} notify gradOn dataset paraMap = do

  -- A map for the momentum gradient
  Mame.withBuffer $ \momentum -> do
    liftIO $ Map.clear momentum -- TODO: necessary?

    -- A worker map for computing the actual gradients
    Mame.withBuffer $ \u -> do

      liftIO $ putStrLn $ "Running momentum!"

      doIt momentum u 0 (R.mkStdGen 0)

  where

    -- Gain in k-th iteration.
    gain k = (gain0 * tau) / (tau + done k)

    -- Number of completed iterations over the full dataset.
    done :: Int -> Double
    done k
        = fromIntegral (k * batchSize)
        / fromIntegral (D.size dataset)

    -- Regularization (Guassian prior) parameter
    regularizationParam = regCoef
      where
        regCoef = iVar ** coef
        iVar = 1.0 / regVar
        coef = fromIntegral (D.size dataset)
             / fromIntegral batchSize

    doIt momentum u k stdGen

      | done k > iterNum = do
          lift $ notify k

      | otherwise = do

          -- Sample the dataset
          (batch, stdGen') <- liftIO $
            D.sample stdGen batchSize dataset

          -- Run notification
          lift $ notify k

          -- Compute the gradient and put it in `u`
          liftIO (Map.clear u)
          lift $ gradOn u batch

          liftIO $ do

            -- Apply regularization to `u`
            applyRegularization regularizationParam paraMap u

            -- Scale the gradient
            scale (gain k) u

            -- Compute the new momentum
            updateMomentum gamma momentum u

            -- Add the momentum gradient to the parameter map
            momentum `addTo` paraMap

          -- Continue turning...
          doIt momentum u (k+1) stdGen'


-- | Apply regularization *to the gradient*.
applyRegularization
  :: (PrimMonad prim, Map prim map k Double)
  => Double       -- ^ Regularization parameter
  -> map k Double -- ^ The parameters
  -> map k Double -- ^ The current gradient
  -> prim ()
applyRegularization regParam params grad =
  Map.mergeWith update grad params
  where
    update x y = x - regParam * y


-- -- | Compute the new momentum (gradient) vector.
-- applyRegularization
--   :: Double -- ^ Regularization parameter
--   -> MVect  -- ^ The parameters
--   -> MVect  -- ^ The current gradient
--   -> IO ()
-- applyRegularization regParam params grad = do
--   forM_ [0 .. UM.length grad - 1] $ \i -> do
--     x <- UM.unsafeRead grad i
--     y <- UM.unsafeRead params i
--     UM.unsafeWrite grad i $ x - regParam * y


-- | Compute the new momentum (gradient) vector.
updateMomentum
  :: (PrimMonad prim, Map prim map k Double)
  => Double       -- ^ The gamma parameter
  -> map k Double -- ^ The previous momentum
  -> map k Double -- ^ The current, scaled gradient
  -> prim ()
updateMomentum gammaCoef momentum grad =
  Map.mergeWith update momentum grad
  where
    update x y = gammaCoef * x + y


-- -- | Compute the new momentum (gradient) vector.
-- updateMomentum
--   :: Double -- ^ The gamma parameter
--   -> MVect  -- ^ The previous momentum
--   -> MVect  -- ^ The scaled current gradient
--   -> IO ()
-- updateMomentum gammaCoef momentum grad = do
--   forM_ [0 .. UM.length momentum - 1] $ \i -> do
--     x <- UM.unsafeRead momentum i
--     y <- UM.unsafeRead grad i
--     UM.unsafeWrite momentum i (gammaCoef * x + y)


-- | Scale the vector by the given value.
scale
  :: (PrimMonad prim, Map prim map k Double)
  => Double
  -> map k Double
  -> prim ()
scale c = Map.modify (c*)


-- -- | Scale the vector by the given value.
-- scale :: Double -> MVect -> IO ()
-- scale c v = do
--   forM_ [0 .. UM.length v - 1] $ \i -> do
--     y <- UM.unsafeRead v i
--     UM.unsafeWrite v i (c * y)


-- | Apply gradient to the parameters vector, that is add the first vector to
-- the second one.
addTo
  :: (PrimMonad prim, Map prim map k Double)
  => map k Double
  -> map k Double
  -> prim ()
addTo w v =
  Map.mergeWith (\x y -> x + y) v w


-- -- | Apply gradient to the parameters vector, that is add the first vector to
-- -- the second one.
-- addTo :: MVect -> MVect -> IO ()
-- addTo w v = do
--   forM_ [0 .. UM.length v - 1] $ \i -> do
--     x <- UM.unsafeRead v i
--     y <- UM.unsafeRead w i
--     UM.unsafeWrite v i (x + y)


------------------------------------------------------------------
-- Obsolete
------------------------------------------------------------------


-- -- | Add up all gradients and store results in normal domain.
-- addUp :: Grad -> MVect -> IO ()
-- addUp grad v = do
--     UM.set v 0
--     forM_ (toList grad) $ \(i, x) -> do
--         y <- UM.unsafeRead v i
--         UM.unsafeWrite v i (x + y)
