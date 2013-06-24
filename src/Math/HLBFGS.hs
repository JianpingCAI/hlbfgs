{-# LANGUAGE ForeignFunctionInterface #-}
{-# CFILES driver.c #-}

-- | Haskell interface for the L-BFGS reference implementation of Nocedal
module Math.HLBFGS
( runSolver
) where

import Foreign.C
import Foreign.Ptr
import Foreign.ForeignPtr
import qualified Data.Vector.Storable as S

--------------------------------------------------------------------------------
-- Data types and aliases ------------------------------------------------------
--------------------------------------------------------------------------------

-- Convenient type alias
type Vec = S.Vector CDouble

-- Constructor-less type representing foreign solver-state structure
data StateStruct

-- Convenient type alias for pointer to foreign structure
type StateStructPtr = Ptr StateStruct

-- Data type for putting it all together
data SolverConfig = SolverConfig CInt StateStructPtr deriving (Show)

--------------------------------------------------------------------------------
-- Interfaces for C-based wrapper calls ----------------------------------------
--------------------------------------------------------------------------------

foreign import ccall "initialize_solver" initWrapper
  :: CInt
  -> CInt
  -> CInt
  -> CDouble
  -> Ptr CDouble
  -> IO StateStructPtr

foreign import ccall "finalize_solver" finalizeWrapper
  :: StateStructPtr
  -> IO ()

foreign import ccall "get_solution_vector" getInternalSolutionVector
  :: StateStructPtr
  -> IO (Ptr CDouble)

foreign import ccall "get_solution_vector_copy" getInternalSolutionVectorCopy
  :: StateStructPtr
  -> IO (Ptr CDouble)

foreign import ccall "&free_solution_vector_copy" freeSolutionVectorCopy
  :: FunPtr (Ptr CDouble -> IO ())

foreign import ccall "update_cost_and_gradient" updateInternalState
  :: StateStructPtr
  -> CDouble
  -> Ptr CDouble
  -> IO ()

foreign import ccall "iterate_solver" iterLBFGS
  :: StateStructPtr
  -> IO CInt

--------------------------------------------------------------------------------
-- Internal support routines ---------------------------------------------------
--------------------------------------------------------------------------------

initializeSolver
  :: Bool                     -- verbositiy @v@
  -> CInt                     -- solution dimension @n@
  -> CInt                     -- memory dimension @m@
  -> CDouble                  -- tolerance @eps@
  -> Vec                      -- initial solution @x0@
  -> IO (Maybe SolverConfig)  -- returns: 'SolverConfig' on success
initializeSolver v n m eps x0 =
  S.unsafeWith x0 (initWrapper iv n m eps) >>= wrap
  where wrap p =  if p == nullPtr
                    then return Nothing
                    else return . Just $ SolverConfig n p
        iv = if v then 1 else 0

finalizeFailure
  :: SolverConfig
  -> IO (Maybe a)
finalizeFailure (SolverConfig _ p) = finalizeWrapper p >> return Nothing

finalizeWithSolution
  :: CInt
  -> Bool
  -> SolverConfig
  -> IO (Maybe (CInt,Bool,Vec))
finalizeWithSolution it converged (SolverConfig n p) = do
  px <- getInternalSolutionVectorCopy p
  finalizeWrapper p
  if px == nullPtr
    then  return Nothing
    else  newForeignPtr freeSolutionVectorCopy px >>= wrap >>=
            \x -> return . Just $ (it,converged,x)
      where wrap fp = return $ S.unsafeFromForeignPtr0 fp (fromIntegral n)

getSolution
  :: SolverConfig
  -> IO Vec
getSolution (SolverConfig n p) =
  getInternalSolutionVector p >>= newForeignPtr_ >>= wrap
  where wrap fp = return $ S.unsafeFromForeignPtr0 fp (fromIntegral n)

updateSolverState
  :: SolverConfig
  -> CDouble
  -> Vec
  -> IO ()
updateSolverState (SolverConfig _ p) f gs =
  S.unsafeWith gs (updateInternalState p f)

iterateSolver
  :: SolverConfig
  -> IO CInt
iterateSolver (SolverConfig _ p) = iterLBFGS p

--------------------------------------------------------------------------------
-- The exported solver routine -------------------------------------------------
--------------------------------------------------------------------------------

-- | Minimize cost function @f@ w.r.t. @x@ using the L-BFGS algorithm
--
-- Returns @Nothing@ on initialization error (e.g. memory allocation) or
-- solution error (e.g. failure in line search). All @Int@ and @Double@
-- arguments are actually @CInt@s and @CDouble@s - the user must handle the
-- explicit conversions themself, but this is trivial with @fromIntegral@ and
-- @realToFrac@.
runSolver
  :: Bool                       -- ^ verbosity (@True@: per-iter, @False@: silent)
  -> CInt                       -- ^ solution dimension @n@
  -> CInt                       -- ^ memory dimension @m@
  -> CInt                       -- ^ max number of iters @niter@
  -> CDouble                    -- ^ tolerance @eps@
  -> Vec                        -- ^ initial solution @x0@
  -> (Vec -> CDouble)           -- ^ const function @f@
  -> (Vec -> Vec)               -- ^ gradient function @g@
  -> IO (Maybe (CInt,Bool,Vec)) -- ^ returns: iter count, converged, solution or @Nothing@
runSolver v n m niter eps x0 f g = initializeSolver v n m eps x0 >>= run
  where run Nothing     = return Nothing
        run (Just conf) =
          let iter it iflag x =
                case iflag of
                  1 ->  if it == niter
                          then finalizeWithSolution it False conf
                          else do
                            updateSolverState conf (f x) (g x)
                            iflag' <- iterateSolver conf
                            xcurr  <- getSolution conf
                            iter (it+1) iflag' xcurr
                  0 ->  finalizeWithSolution it True conf
                  _ ->  finalizeFailure conf
          in  iter 0 1 x0
