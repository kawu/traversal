module NLP.Traversal.CRF.Mame
(
-- * Buffers
  Mame
, Make (..)
, runMame
, withValueBuffer
, withDoubleBuffer
) where


import           Control.Monad.Trans.Class (lift)
import qualified Control.Monad.RWS.Strict as RWS


------------------------------------------------------------------
-- Manual memory management monad
------------------------------------------------------------------


-- | Buffer creation functions. There are two types of buffers: with `v` values
-- and with `Double` values. Keys are always of type `f` and they correspond to
-- model parameters. Buffers should cover all the possible model features (i.e.,
-- they should be defined for all features in the model).
data Make map f v m = Make
  { mkValueBuffer :: m (map f v)
  , mkDoubleBuffer :: m (map f Double)
  }


-- | Buffer stacks.
data Stacks map f v = Stacks
  { valueStack  :: [map f v]
  , doubleStack :: [map f Double]
  }


-- | Manual memory management monad transformer.
type Mame map f v m =
  RWS.RWST
  (Make map f v m)
  ()
  (Stacks map f v)
  m


-- | Run a memory management monadic action.
runMame
  :: Monad m
  => Make map f v m
  -> Mame map f v m a
  -> m a
runMame make action = fst <$> RWS.evalRWST action make (Stacks [] [])


------------------------------------------------------------------
-- Value buffers
------------------------------------------------------------------


-- | Pop a buffer.  Create one if doesn't exist.
popValueBuffer
  :: Monad m
  => Mame map f v m (map f v)
popValueBuffer = do
  free <- RWS.gets valueStack
  case free of
    buf : rest -> do
      RWS.modify' $ \s ->
        s {valueStack = rest}
      return buf
    _ -> do
      new <- RWS.asks mkValueBuffer
      lift new


-- | Return the buffer on the stack. Internal function.
pushValueBuffer
  :: Monad m
  => map f v
  -> Mame map f v m ()
pushValueBuffer buf = RWS.modify' $ \s ->
  s {valueStack = buf : valueStack s}


-- | Perform a computation with a buffer.
--
-- WARNING: the computation should not return the buffer nor any direct or
-- indirect references to it. `Mame` may allocate it to other workers once the
-- computation is over.
withValueBuffer
  :: (Monad m)
  => (map f v -> Mame map f v m a)
  -> Mame map f v m a
withValueBuffer f = do
  buf <- popValueBuffer
  x <- f buf
  pushValueBuffer buf
  return x


------------------------------------------------------------------
-- Double buffers
------------------------------------------------------------------


-- | Pop a buffer.  Create one if doesn't exist.
popDoubleBuffer
  :: Monad m
  => Mame map f v m (map f Double)
popDoubleBuffer = do
  free <- RWS.gets doubleStack
  case free of
    buf : rest -> do
      RWS.modify' $ \s ->
        s {doubleStack = rest}
      return buf
    _ -> do
      new <- RWS.asks mkDoubleBuffer
      lift new


-- | Return the buffer on the stack. Internal function.
pushDoubleBuffer
  :: Monad m
  => map f Double
  -> Mame map f v m ()
pushDoubleBuffer buf = RWS.modify' $ \s ->
  s {doubleStack = buf : doubleStack s}


-- | Perform a computation with a buffer.
--
-- WARNING: the computation should not return the buffer nor any direct or
-- indirect references to it. `Mame` may allocate it to other workers once the
-- computation is over.
withDoubleBuffer
  :: (Monad m)
  => (map f Double -> Mame map f v m a)
  -> Mame map f v m a
withDoubleBuffer f = do
  buf <- popDoubleBuffer
  x <- f buf
  pushDoubleBuffer buf
  return x
