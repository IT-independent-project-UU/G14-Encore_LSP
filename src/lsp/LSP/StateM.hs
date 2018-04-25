{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module LSP.StateM (
    StateM(runStateM),
    stateM,
    get,
    put,
    modify,
    sequenceStateM
) where

import Control.Monad.State (MonadState, get, put, modify, state)

newtype StateM s (m :: * -> *) a = StateM {
    runStateM :: s -> m (a, s)
}

stateM :: Monad m => (s -> m (a, s)) -> StateM s m a
stateM = StateM

sequenceStateM :: Monad m => (a -> StateM s m b) -> [a] -> m s -> [m (b, s)]
sequenceStateM _ [] _ = []
sequenceStateM f (a:bs) s =
    this : sequenceStateM f bs (this >>= (return . snd))
    where this = s >>= (\s -> runStateM (f a) s)

instance Monad m => Monad (StateM s m) where
    (StateM a) >>= f =
        stateM (\s -> do (a, s) <- a s
                         case f a of
                            StateM b -> b s)


instance Monad m => Applicative (StateM s m) where
    pure a = stateM (\s -> return (a, s))
    (StateM a) <*> (StateM b) =
        stateM (\s -> do (b, s2) <- b s
                         (f, _)  <- a s
                         return (f b, s2))

instance Monad m => Functor (StateM s m) where
    fmap f (StateM a) = stateM $ fmap (\(a, s) -> (f a, s)) . a

instance Monad m => MonadState s (StateM s m) where
    get = stateM (\s -> return (s, s))
    put s = stateM (\_ -> return ((), s))
