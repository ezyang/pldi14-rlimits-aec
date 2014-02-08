{-# LANGUAGE RankNTypes, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, OverlappingInstances#-}
import Control.Monad (liftM, replicateM)

class Monad m => MonadState s m where
    get :: m s
    put :: s -> m ()

modify :: MonadState s m => (s -> s) -> m ()
modify f = get >>= put . f

newtype StateT s m a = StateT { getStateTFunc
                                    :: forall r . s -> m ((a -> s -> r) -> r)}

instance Monad m => Monad (StateT s m) where
    return x = StateT $ \s -> return $ \f -> f x s
    StateT f >>= g = StateT $ \s -> do
        useX <- f s
        useX $ \x s' -> getStateTFunc (g x) s'

runStateT :: Monad m => StateT s m a -> s -> m (a, s)
runStateT f s = do
    useXS <- getStateTFunc f s
    return $ useXS $ \x s' -> (x,s')

instance Monad m => MonadState s (StateT s m) where
    get = StateT $ \s -> return $ \f -> f s s
    put s = s `seq` StateT $ \_ -> return $ \f -> f () s

-- benchmark
type LargeState = StateT Int (
                  StateT Int (
                  StateT Int (
                  StateT Int (
                  StateT Int (
                  StateT Int (
                  StateT Int (
                  StateT Int (
                  StateT Int (
                  StateT Int (
                  IO
                  ))))))))))

incrementLevel0 :: LargeState Int
incrementLevel0 = do
    modify inc
    get

inc :: Int -> Int
inc n = n + 1
{-# INLINE inc #-}

runLargeState :: LargeState a -> IO a
runLargeState s = do
    let s0 = liftM fst $ runStateT s 0
    let s1 = liftM fst $ runStateT s0 0
    let s2 = liftM fst $ runStateT s1 0
    let s3 = liftM fst $ runStateT s2 0
    let s4 = liftM fst $ runStateT s3 0
    let s5 = liftM fst $ runStateT s4 0
    let s6 = liftM fst $ runStateT s5 0
    let s7 = liftM fst $ runStateT s6 0
    let s8 = liftM fst $ runStateT s7 0
    liftM fst $ runStateT s8 0

main :: IO ()
main = do
    s <- runLargeState $ replicateM 1000 incrementLevel0
    putStrLn $ show s
