module NLP.Departage.CRF.Mame
(
-- * Buffers
  Mame
, runMame
, withBuffer
) where


import           Control.Monad.Trans.Class (lift)
import qualified Control.Monad.RWS.Strict as RWS


------------------------------------------------------------------
-- Manual memory management monad
------------------------------------------------------------------


-- | Manual memory management monad transformer. All maps in provided by `Mame`
-- have the same size and index span.
type Mame buf m =
  RWS.RWST
  (m buf) -- ^ Monadic buffer creation
  ()
  [buf]   -- ^ Stack of existing, free buffers
  m


-- | Run a memory management monadic action.
runMame :: Monad m => m buf -> Mame buf m a -> m a
runMame new action = fst <$> RWS.evalRWST action new []


-- | Pop or create a new buffer. Internal function.
popBuffer :: Monad m => Mame buf m buf
popBuffer = do
  free <- RWS.get
  case free of
    buf : rest -> do
      RWS.put rest
      return buf
    _ -> do
      new <- RWS.ask
      lift new


-- | Return the buffer on the stack. Internal function.
pushBuffer :: Monad m => buf -> Mame buf m ()
pushBuffer buf = RWS.modify' (buf:)


-- | Perform a computation with a buffer.
--
-- WARNING: the computation should not return the buffer nor any direct or
-- indirect references to it. `Mame` may allocate it to other workers once the
-- computation is over.
withBuffer
  :: (Monad m)
  => (buf -> Mame buf m a)
  -> Mame buf m a
withBuffer f = do
  buf <- popBuffer
  x <- f buf
  pushBuffer buf
  return x
