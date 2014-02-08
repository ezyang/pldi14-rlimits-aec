import Data.IORef
import Data.Maybe
import Data.List
import Control.Exception
import Control.Monad
import System.Exit
import System.Environment
import System.IO
import System.Mem
import Control.Concurrent
import Foreign.C.String
import Foreign.StablePtr
import GHC.Stats
import Data.Array.IO
import Data.Word

import Control.RLimits

import GHC
import GHC.Paths (libdir)
import DynFlags

-- SOME CLASSIC SPACE LEAKS

qes = flip seq

pure_handler :: (Int -> a) -> Int -> IO ()
pure_handler f n = do
    let r = f n
    -- newStablePtr r -- keep it live
    evaluate r
    return ()

-- retainer
-- n = 20000
f0 = pure_handler $ \n -> let xs = [1..fromIntegral n::Integer] in sum xs * product xs

-- opl
-- n = 9
opl n = find vw $ map (\x-> fromDigits (x++[0,0,n]) ) 
        $ sequence [[1],re,[2],re,[3],re,[4],re,[5],re,[6],re,[7],re,[n-1],re]
vw x = hh^2 == x
    where hh = (round.sqrt.fromIntegral) x
re = [0..9]
fromDigits x = foldl1 (\n m->10*n+m) x
f1 = pure_handler $ \n -> fromJust $ opl (fromIntegral n :: Integer)

-- n = 2000000
leaky_lines    :: String -> [String]
leaky_lines "" =  []
leaky_lines s  =  let (l, s') = break (== '\n') s
                  in  l : case s' of
                               []      -> []
                               (_:s'') -> leaky_lines s''
f2 = pure_handler $ \n -> length (leaky_lines (replicate n 'a' ++ "\n" ++ replicate n 'b'))


-- n = 20000
-- suml
mysum :: [Int] -> Int
mysum = foldr (+) 0
f3 = pure_handler $ \n -> mysum [1..fromIntegral n]

-- n = 20000000
-- tree
data MyTree = MyNode [MyTree] | MyLeaf [Int]

makeTree :: Int -> Int -> MyTree
makeTree i 0 = MyLeaf [0..i]
makeTree i n = MyNode [ makeTree i (n - 1)
                  , makeTree i (n - 1) ]

count2 :: MyTree -> MyTree -> Int
count2 r (MyNode xs) = 1 + sum (map (count2 r) xs)
count2 r (MyLeaf xs) = length xs
f4 = pure_handler $ \n -> let r = makeTree (2*n) n in count2 r r

-- small allocation (use product xs to retain)
f5 = pure_handler $ \n -> let xs = [n..] in evaluate $ sum xs * product xs

-- sub-block large allocation
f6 n = replicateM n ((newArray (0, 500) True) :: IO (IOArray Int Bool)) >>= newStablePtr >> return ()

-- ghc api

compile_ghc fn = do
    runGhc (Just libdir) $ do
        dflags <- getSessionDynFlags
        setSessionDynFlags (gopt_set (updOptLevel 2 dflags) Opt_ForceRecomp)
        target <- guessTarget fn Nothing
        setTargets [target]
        load LoadAllTargets
    return ()

f7 _ = do
    compile_ghc "cont-state8-cleaned-up.hs"
    return ()

-- block large allocation
f8 n = replicateM n ((newArray (0, 5000) True) :: IO (IOArray Int Bool)) >>= newStablePtr >> return ()

-- megablock large allocation
f9 n = replicateM n ((newArray (0, 131072) True) :: IO (IOArray Int Bool)) >>= newStablePtr >> return ()

fs :: [Int -> IO ()]
fs = [f0,f1,f2,f3,f4,f5,f6,f7,f8,f9]

main = do
    args <- getArgs
    let t = read (args !! 0) :: Int -- which test
        n = read (args !! 1) :: Int -- how big is the input size
        p = read (args !! 2) :: Word -- how many pages should the rlimit be?
    parent <- getCurrentRC ()
    x <- newRC p parent
    nr <- newIORef n
    let handler = do
        performGC
        stats <- getGCStats
        print . maxBytesUsed $ stats
        print . (*(1024*1024)) . peakMegabytesAllocated $ stats -- for simplicity
        exitSuccess
    -- l <- listenRC x p handler
    when (t < 0) handler
    let f = fs !! t
    m <- newEmptyMVar
    (withRC x $ do
        n <- readIORef nr
        f n
        exitFailure)
        `catch` (\HeapOverflow -> handler)
    exitFailure
