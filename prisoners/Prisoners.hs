{-# LANGUAGE NoMonomorphismRestriction, DeriveDataTypeable #-}

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Concurrent.Async
import Control.Monad
import Control.Exception
import System.Timeout
import Data.IORef
import Data.List
import Data.Array.IO
import Data.Typeable
import System.Mem
import Data.Word

import Control.RLimits

-- Prisoner's dilemma in Haskell
--
-- Architecture:
--      A prisoner is a thread running in a monad M. It is
--      given a function: play :: Bool -> M Bool, which indicates
--      its decision whether or not to defect (False) or cooperate
--      (True), and
--      information about whether or not its opponent defected
--      or cooperated.
--
--      The thread is permitted to maintain state as it is running.
--
-- Strategies:
--      Tit for tat: Do what was done previously
--      Random: randomly cooperate or defect [ GOT RID OF: TOO SLOW ]
--      Tit for tat with memory: Tit for tat player who also remembers
--          everything that happened previously (he will hit a resource
--          limit)

type M = IO

p1 play = play True >>= f
    where f prev = play prev >>= f

p2 play = play True >>= f
    where f prev = play prev >>= f >> play False >> return ()

p3 play = play False >> p3 play

p4 play = play True >> p4 play

play cin cout x = x `seq` putMVar cout x >> takeMVar cin

-- parameters
t = 4 -- defect/cooperate
r = 3 -- cooperate/cooperate
p = 2 -- defect/defect
s = 1 -- cooperate/defect

score True True = r
score True False = s
score False True = t
score False False = p

data E = E Int
    deriving (Show, Typeable)
instance Exception E

handler mv i m = m `catch` h
    where h HeapOverflow = putMVar mv ()
          h _ = return ()

main = do
    let players = [p1, p2, p3, p4] :: [(Bool -> IO Bool) -> IO ()]
    scores <- mapM (const (newIORef 0)) players
    parent_rc <- getCurrentRC ()
    forM_ (zip3 [1..] players scores) $ \(i,p,s) -> do
        forM_ (zip3 [1..] players scores) $ \(i',p',s') -> do
            when (i /= i') $ do
            cin <- newEmptyMVar
            cin' <- newEmptyMVar
            cout <- newEmptyMVar
            cout' <- newEmptyMVar
            dead <- newEmptyMVar
            dead' <- newEmptyMVar
            rc <- newRC 500 parent_rc
            rc' <- newRC 500 parent_rc
            t <- forkIO . handler dead i . withRC rc $ p (play cin cout)
            t' <- forkIO . handler dead' i' . withRC rc' $ p' (play cin' cout')
            run1 <- async . replicateM_ 500000 $ do
                r <- takeMVar cout
                r' <- takeMVar cout'
                putMVar cin r'
                putMVar cin' r
                modifyIORef' s (+ score r r')
                modifyIORef' s' (+ score r' r)
            run2 <- async (takeMVar dead)
            run3 <- async (takeMVar dead')
            waitAnyCancel [run1, run2, run3]
            killThread t
            killThread t'
            killRC rc
            killRC rc'
            -- final_scores <- mapM readIORef scores
            -- print final_scores
            performGC
            performGC
            performGC
