{-# LANGUAGE Arrows     #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module Main where

import qualified Control.Concurrent.ReadWriteLock as RWL
import GHC.Generics (Generic)
import Control.DeepSeq
import Control.Concurrent.MVar
import Data.IORef
import Control.Concurrent
import Control.Monad.Random
import Control.Monad.Reader
import Control.Monad.Trans.MSF.Random
-- import Data.MonadicStreamFunction.InternalCore
import Data.Array.IArray
import FRP.BearRiver
import qualified Criterion.Main as Crit

data SIRState = Susceptible | Infected | Recovered 
  deriving (Show, Eq, Generic, NFData)

type Disc2dCoord  = (Int, Int)
type Dimension    = (Int, Int)
type SIREnv       = Array Disc2dCoord SIRState
type SIRMonad g   = RandT g IO
type SIRAgent g   = SF (SIRMonad g) () ()

contactRate :: Double
contactRate = 5.0

infectivity :: Double
infectivity = 0.05

illnessDuration :: Double
illnessDuration = 15.0

rngSeed :: Int
rngSeed = 42

main :: IO ()
main = do
    let dt = 0.1
        t  = 100
        g  = mkStdGen rngSeed
    
    cores <- getNumCapabilities

    Crit.defaultMain [
        Crit.bgroup ("sir-io-rw-cores:" ++ show cores)
        [ Crit.bench "51x51"   $ Crit.nfIO (initSim g t dt ( 51,  51)) ]
      , Crit.bgroup ("sir-io-rw-agents:" ++ show cores)
        [ Crit.bench "101x101" $ Crit.nfIO (initSim g t dt (101, 101))
        , Crit.bench "151x151" $ Crit.nfIO (initSim g t dt (151, 151))
        , Crit.bench "201x201" $ Crit.nfIO (initSim g t dt (201, 201))
        , Crit.bench "251x251" $ Crit.nfIO (initSim g t dt (251, 251)) ]
      ]
  where
    initSim g t dt d = do
      let (as, e) = initAgentsEnv d
      runSimulation g t dt e as d

runSimulation :: RandomGen g
              => g 
              -> Time 
              -> DTime 
              -> SIREnv
              -> [(Disc2dCoord, SIRState)] 
              -> Dimension
              -> IO [SIREnv]
runSimulation g0 t dt e as d = do
    env <- newIORef e
    rwl <- RWL.new

    let n         = length as
        (rngs, _) = rngSplits g0 n []
        steps     = floor $ t / dt

    vars <- zipWithM (\g' a -> do
      dtVar  <- newEmptyMVar 
      retVar <- createAgentThread steps env rwl dtVar g' a d
      return (dtVar, retVar)) rngs as

    let (dtVars, retVars) = unzip vars

    forM [0..steps-1] (simulationStep env dtVars retVars)

  where
    rngSplits :: RandomGen g => g -> Int -> [g] -> ([g], g)
    rngSplits g 0 acc = (acc, g)
    rngSplits g n acc = rngSplits g'' (n-1) (g' : acc)
      where
        (g', g'') = split g

    simulationStep :: IORef SIREnv
                   -> [MVar DTime]
                   -> [MVar ()]
                   -> Int
                   -> IO SIREnv
    simulationStep env dtVars retVars _i = do
      --putStrLn $ "Step " ++ show _i

      -- tell all threads to continue with the corresponding DTime
      mapM_ (`putMVar` dt) dtVars
      -- wait for results
      mapM_ takeMVar retVars
      -- read last version of environment
      readIORef env

createAgentThread :: RandomGen g 
                  => Int 
                  -> IORef SIREnv
                  -> RWL.RWLock
                  -> MVar DTime
                  -> g
                  -> (Disc2dCoord, SIRState)
                  -> Dimension
                  -> IO (MVar ())
createAgentThread steps env rwl dtVar rng0 a d = do
    let sf = uncurry (sirAgent env rwl d) a
    -- create the var where the result will be posted to
    retVar <- newEmptyMVar
    _ <- forkIO $ agentThread steps sf rng0 retVar
    return retVar
  where
    agentThread :: RandomGen g 
                => Int
                -> SIRAgent g
                -> g
                -> MVar ()
                -> IO ()
    agentThread 0 _ _ _ = return ()
    agentThread n sf rng retVar = do
      -- wait for next dt to compute next step
      dt <- takeMVar dtVar

      -- compute next step
      let sfReader = unMSF sf ()
          sfRand   = runReaderT sfReader dt
          sfSTM    = runRandT sfRand rng
      ((_, sf'), rng') <- sfSTM
      -- NOTE: running STM with stats results in considerable lower performance the more STM actions are run concurrently

      -- post result to main thread
      putMVar retVar ()
      
      agentThread (n - 1) sf' rng' retVar

sirAgent :: RandomGen g 
         => IORef SIREnv
         -> RWL.RWLock
         -> Dimension
         -> Disc2dCoord 
         -> SIRState 
         -> SIRAgent g
sirAgent env rwl d c Susceptible = susceptibleAgent env rwl c d
sirAgent env rwl _ c Infected    = infectedAgent env rwl c 
sirAgent _   _   _ _ Recovered   = recoveredAgent

susceptibleAgent :: RandomGen g 
                 => IORef SIREnv 
                 -> RWL.RWLock
                 -> Disc2dCoord
                 -> Dimension
                 -> SIRAgent g
susceptibleAgent env rwl coord d = 
    switch
      susceptible
      (const $ infectedAgent env rwl coord)
  where
    susceptible :: RandomGen g 
                => SF (SIRMonad g) () ((), Event ())
    susceptible = proc _ -> do
      makeContact <- occasionally (1 / contactRate) () -< ()

      if not $ isEvent makeContact 
        then returnA -< ((), NoEvent)
        else (do
          -- read the environment, trivial in this case: aquire read lock
          -- the resulting immutable environment must only be used for reading
          -- therefore cannot be changed and used to update the IORef later
          -- as it would potentially overwrite changes of other agents!
          -- aquire read lock
          arrM_ (liftIO $ RWL.acquireRead rwl) -< ()
          -- read (immutable) shared environment data
          eReadOnly <- arrM_ (liftIO $ readIORef env) -< ()
          -- release read lock
          arrM_ (liftIO $ RWL.releaseRead rwl) -< ()
          let ns = neighbours eReadOnly coord d moore
          --let ns = allNeighbours e
          s <- drawRandomElemS -< ns
          case s of
            Infected -> do
              infected <- arrM_ (lift $ randomBool infectivity) -< ()
              if infected 
                then (do
                  -- here we are writing the environment after updating it.
                  -- it is crucial to read the environment again and NOT using
                  -- the eReadOnly to updated it because other threads could
                  -- have update the environment in the meantime.
                  -- note that is no problem to read the environment when
                  -- holding the write lock because a writer has always
                  -- exclusive access
                  -- aquire write lock
                  arrM_ (liftIO $ RWL.acquireWrite rwl) -< ()
                  -- read (immutable) shared environment data (again)
                  e <- arrM_ (liftIO $ readIORef env) -< ()
                  -- change environment data
                  let e' = changeCell coord Infected e
                  -- write environment data
                  arrM (liftIO . writeIORef env) -< e'
                  -- release write lock
                  arrM_ (liftIO $ RWL.releaseWrite rwl) -< ()
                  returnA -< ((), Event ()))
                else returnA -< ((), NoEvent)
            _       -> returnA -< ((), NoEvent))

infectedAgent :: RandomGen g 
              => IORef SIREnv 
              -> RWL.RWLock
              -> Disc2dCoord
              -> SIRAgent g
infectedAgent env rwl coord = 
    switch
    infected 
      (const recoveredAgent)
  where
    infected :: RandomGen g => SF (SIRMonad g) () ((), Event ())
    infected = proc _ -> do
      recovered <- occasionally illnessDuration () -< ()
      if isEvent recovered
        then (do
          -- trivial case: unconditionally write (change) shared environment data
          -- aquire write lock
          arrM_ (liftIO $ RWL.acquireWrite rwl) -< ()
          -- write environment
          arrM_ (liftIO $ modifyIORef env (changeCell coord Recovered)) -< ()
          -- release write lock
          arrM_ (liftIO $ RWL.releaseWrite rwl) -< ()
          returnA -< ((), Event ()))
        else returnA -< ((), NoEvent)

recoveredAgent :: RandomGen g => SIRAgent g
recoveredAgent = returnA 

drawRandomElemS :: MonadRandom m => SF m [a] a
drawRandomElemS = proc as -> do
  r <- getRandomRS ((0, 1) :: (Double, Double)) -< ()
  let len = length as
  let idx = fromIntegral len * r
  let a =  as !! floor idx
  returnA -< a

randomBoolM :: RandomGen g => Double -> Rand g Bool
randomBoolM p = getRandomR (0, 1) >>= (\r -> return $ r <= p)

randomBool :: MonadRandom m => Double -> m Bool
randomBool p = getRandomR (0, 1) >>= (\r -> return $ r <= p)

changeCell :: Disc2dCoord -> SIRState -> SIREnv -> SIREnv
changeCell c s e = e // [(c, s)]

neighbours :: SIREnv 
           -> Disc2dCoord 
           -> Disc2dCoord
           -> [Disc2dCoord] 
           -> [SIRState]
neighbours e (x, y) (dx, dy) n = map (e !) nCoords'
  where
    nCoords = map (\(x', y') -> (x + x', y + y')) n
    nCoords' = filter (\(xn, yn) -> xn >= 0 && 
                                    yn >= 0 && 
                                    xn <= (dx - 1) &&
                                    yn <= (dy - 1)) nCoords
allNeighbours :: SIREnv -> [SIRState]
allNeighbours = elems

neumann :: [Disc2dCoord]
neumann = [ topDelta, leftDelta, rightDelta, bottomDelta ]

moore :: [Disc2dCoord]
moore = [ topLeftDelta,    topDelta,     topRightDelta,
          leftDelta,                     rightDelta,
          bottomLeftDelta, bottomDelta,  bottomRightDelta ]

topLeftDelta :: Disc2dCoord
topLeftDelta      = (-1, -1)
topDelta :: Disc2dCoord
topDelta          = ( 0, -1)
topRightDelta :: Disc2dCoord
topRightDelta     = ( 1, -1)
leftDelta :: Disc2dCoord
leftDelta         = (-1,  0)
rightDelta :: Disc2dCoord
rightDelta        = ( 1,  0)
bottomLeftDelta :: Disc2dCoord
bottomLeftDelta   = (-1,  1)
bottomDelta :: Disc2dCoord
bottomDelta       = ( 0,  1)
bottomRightDelta :: Disc2dCoord
bottomRightDelta  = ( 1,  1)

-------------------------------------------------------------------------------
environmentsToAgentDyns :: [SIREnv] -> [[SIRState]]
environmentsToAgentDyns = map elems

initAgentsEnv :: (Int, Int) -> ([(Disc2dCoord, SIRState)], SIREnv)
initAgentsEnv (xd, yd) = (as, e)
  where
    xCenter = floor $ fromIntegral xd * (0.5 :: Double)
    yCenter = floor $ fromIntegral yd * (0.5 :: Double)
    
    sus = [ ((x, y), Susceptible) | x <- [0..xd-1], 
                                    y <- [0..yd-1],
                                    x /= xCenter ||
                                    y /= yCenter ] 
    inf = ((xCenter, yCenter), Infected)
    as = inf : sus

    e = array ((0, 0), (xd - 1, yd - 1)) as
-------------------------------------------------------------------------------