{-# LANGUAGE BangPatterns #-}

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad
import Control.Exception
import System.Timeout
import Data.IORef
import Data.List
import Data.Array.IO
import Data.Typeable
import System.Mem
import Data.Word

import Control.Monad.RLimits
import Data.SafeCopy

-- Prisoner's dilemma in Haskell
--      Variant using the CM monad API
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

type M = CM

p1 play = play True >>= f
    where f prev = play prev >>= f

p2 play = play True >>= f
    where f prev = play prev >>= f >> play False >> return ()

p3 play = play False >> p3 play

p4 play = play True >> p4 play

-- Since players may hold references to the control thread, no copying
-- is necessary here!  (If the control thread dies, we're in trouble
-- anyway)
play cin cout x = putRCMVar cout x >> takeRCMVar cin

-- parameters
t = 4 -- defect/cooperate
r = 3 -- cooperate/cooperate
p = 2 -- defect/defect
s = 1 -- cooperate/defect

score True True = r
score True False = s
score False True = t
score False False = p

main = startCM program

croak = liftIOTCB . putStrLn
cleanup = liftIOTCB $ replicateM_ 2 performGC

-- This example uses 'RCMVar's, which are to 'MVars' as 'RCRefs' are to
-- 'IORefs'.  They're not in the paper, but they work as you'd expect.
program = do
    let players = zip [1..] ([p1, p2, p3, p4] :: [(Bool -> M Bool) -> M ()])
    c <- getCurrentLabel
    forM_ players $ \(i,p) -> do
        forM_ players $ \(i',p') -> do
            when (i /= i') $ do
            rc <- newRC 500
            rc' <- newRC 500
            rcf <- newRC 500
            cin <- newEmptyRCMVar (ss rcf `su` c)
            cin' <- newEmptyRCMVar (ss rcf `su` c)
            cout <- newEmptyRCMVar (ss rcf `su` ss rc `su` c)
            cout' <- newEmptyRCMVar (ss rcf `su` ss rc' `su` c)
            flag <- newEmptyRCMVar (ss rcf `su` c)
            -- Start off the player processes
            t  <- rcFork (ss rc  `su` c) . withRC rc  $ p  (play cin  cout)
            t' <- rcFork (ss rc' `su` c) . withRC rc' $ p' (play cin' cout')
            -- Here, we use a little idiom for blocking on the completion
            -- of a thread, which in the case of an infinitely looping
            -- thread, indicates that the thread ran out of resource
            -- limits.
            rcFork (ss rcf `su` c) . withRC rcf $ do
                rcCopyResult t trPrim
                croak ("player " ++ show i ++ " killed!")
                putRCMVar flag LT
            rcFork (ss rcf `su` c) . withRC rcf $ do
                rcCopyResult t' trPrim
                croak ("player " ++ show i' ++ " killed!")
                putRCMVar flag GT
            -- Notice that the control thread also is given its
            -- own resource container...
            rcFork (ss rcf `su` c) . withRC rcf $ do
                let loop 0 x x' = return (x, x')
                    -- ...if you remove the bang-patterns here, you
                    -- get a space leak, and the control thread is
                    -- blamed, as you would expect!
                    loop n !x !x' = do
                        r <- copyRCMVar cout trPrim
                        r' <- copyRCMVar cout' trPrim
                        putRCMVar cin r'
                        putRCMVar cin' r
                        loop (n-1) (x + score r r') (x' + score r' r)
                (x, x') <- loop 500000 0 0
                putRCMVar flag (compare x x')
            result <- copyRCMVar flag trPrim
            croak ("result: " ++ show i ++ " " ++ show result ++ " " ++ show i')
            rcKill rc
            rcKill rc'
            -- This kills any control threads which may still be waiting
            -- on MVars which are now dead.
            rcKill rcf
            -- Just a little trick to encourage killed resource
            -- containers to go away, not strictly necessary.
            cleanup
