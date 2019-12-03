{-# LANGUAGE Arrows     #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module Main where

import GHC.Generics (Generic)
import Control.DeepSeq
import Control.Concurrent
import Control.Concurrent.STM
--import Control.Concurrent.STM.Stats
import Control.Monad.Random
import Control.Monad.Reader
import Control.Monad.Trans.MSF.Random
import Data.Array.IArray
import FRP.BearRiver
import qualified Criterion.Main as Crit

data SIRState = Susceptible | Infected | Recovered
  deriving (Show, Eq, Generic, NFData)

type Disc2dCoord  = (Int, Int)
type Dimension    = (Int, Int)
type SIREnv       = Array Disc2dCoord SIRState
type SIRMonad g   = RandT g STM
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
        Crit.bgroup "sir-var-cores"
        [ Crit.bench ("51x51:"   ++ show cores) $ Crit.nfIO (initSim g t dt ( 51,  51)) ]
      , Crit.bgroup "sir-var-agents"
        [ Crit.bench ("51x51:"   ++ show cores) $ Crit.nfIO (initSim g t dt ( 51,  51))
        , Crit.bench ("101x101:" ++ show cores) $ Crit.nfIO (initSim g t dt (101, 101))
        , Crit.bench ("151x151:" ++ show cores) $ Crit.nfIO (initSim g t dt (151, 151))
        , Crit.bench ("201x201:" ++ show cores) $ Crit.nfIO (initSim g t dt (201, 201))
        , Crit.bench ("251x251:" ++ show cores) $ Crit.nfIO (initSim g t dt (251, 251)) ]
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
    -- NOTE: initially I was thinking about using a TArray to reduce the transaction retries
    -- but using a single environment seems to fast enough for now
    env <- newTVarIO e

    let n         = length as
        (rngs, _) = rngSplits g0 n []
        steps     = floor $ t / dt

    vars <- zipWithM (\g' a -> do
      dtVar  <- newEmptyMVar 
      retVar <- createAgentThread steps env dtVar g' a d
      return (dtVar, retVar)) rngs as

    let (dtVars, retVars) = unzip vars

    forM [0..steps-1] (simulationStep env dtVars retVars)

  where
    rngSplits :: RandomGen g => g -> Int -> [g] -> ([g], g)
    rngSplits g 0 acc = (acc, g)
    rngSplits g n acc = rngSplits g'' (n-1) (g' : acc)
      where
        (g', g'') = split g

    simulationStep :: TVar SIREnv
                   -> [MVar DTime]
                   -> [MVar ()]
                   -> Int
                   -> IO SIREnv
    simulationStep env dtVars retVars _i = do
      -- putStrLn $ "Step " ++ show i

      -- tell all threads to continue with the corresponding DTime
      mapM_ (`putMVar` dt) dtVars
      -- wait for results
      mapM_ takeMVar retVars
      -- read last version of environment
      readTVarIO env

createAgentThread :: RandomGen g 
                  => Int 
                  -> TVar SIREnv
                  -> MVar DTime
                  -> g
                  -> (Disc2dCoord, SIRState)
                  -> Dimension
                  -> IO (MVar ())
createAgentThread steps env dtVar rng0 a d = do
    let sf = uncurry (sirAgent env d) a
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
      ((_, sf'), rng') <- atomically sfSTM -- atomically sfSTM -- trackSTM sfSTM
      -- NOTE: running STM with stats results in considerable lower performance the more STM actions are run concurrently

      -- post result to main thread
      putMVar retVar ()
      
      agentThread (n - 1) sf' rng' retVar

sirAgent :: RandomGen g 
         => TVar SIREnv
         -> Dimension
         -> Disc2dCoord 
         -> SIRState 
         -> SIRAgent g
sirAgent env d c Susceptible = susceptibleAgent env c d
sirAgent env _ c Infected    = infectedAgent env c 
sirAgent _   _ _ Recovered   = recoveredAgent

susceptibleAgent :: RandomGen g 
                 => TVar SIREnv 
                 -> Disc2dCoord
                 -> Dimension
                 -> SIRAgent g
susceptibleAgent env coord d = 
    switch 
      susceptible
      (const $ infectedAgent env coord)
  where
    susceptible :: RandomGen g 
                => SF (SIRMonad g) () ((), Event ())
    susceptible = proc _ -> do
      makeContact <- occasionally (1 / contactRate) () -< ()

      if not $ isEvent makeContact 
        then returnA -< ((), NoEvent)
        else (do
          e <- arrM_ (lift $ lift $ readTVar env) -< ()
          let ns = neighbours e coord d moore
          --let ns = allNeighbours e
          s <- drawRandomElemS       -< ns
          case s of
            Infected -> do
              infected <- arrM_ (lift $ randomBool infectivity) -< ()
              if infected 
                then (do
                  let e' = changeCell coord Infected e
                  arrM (lift . lift . writeTVar env) -< e'
                  returnA -< ((), Event ()))
                else returnA -< ((), NoEvent)
            _       -> returnA -< ((), NoEvent))

infectedAgent :: RandomGen g 
              => TVar SIREnv 
              -> Disc2dCoord
              -> SIRAgent g
infectedAgent env coord = 
    switch
    infected 
      (const recoveredAgent)
  where
    infected :: RandomGen g => SF (SIRMonad g) () ((), Event ())
    infected = proc _ -> do
      recovered <- occasionally illnessDuration () -< ()
      if isEvent recovered
        then (do
          arrM_ (lift $ lift $ modifyTVar env (changeCell coord Recovered)) -< ()
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