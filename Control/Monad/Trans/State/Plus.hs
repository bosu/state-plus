{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances, MultiParamTypeClasses #-}
----------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Trans.State.Plus
-- Copyright   :  (c) Boris Sukholitko 2012
-- License     :  BSD3
-- 
-- Maintainer  :  boriss@gmail.com
-- Stability   :  experimental
-- 
-- MonadPlus with left catch (MonadOr) for StateT.
----------------------------------------------------------------------
module Control.Monad.Trans.State.Plus (
    -- * The StatePlusT monad transformer
    StatePlusT, runStatePlusT, execStatePlusT, evalStatePlusT) where

import Control.Monad.State
import Control.Applicative

-- | StatePlusT behaves similar to StateT monad transformer
newtype StatePlusT s m a = MkSPT { unSPT :: StateT (Bool, s) m a }
                                deriving (Functor, MonadTrans, MonadIO)

mzeroError :: a
mzeroError = error "StatePlusT mzero value"

instance Monad m => Monad (StatePlusT s m) where
    return = lift . return
    x >>= f = (MkSPT . StateT) go
        where go s = do
                    (a, s') <- runStateT (unSPT x) s
                    if fst s'
                            then runStateT (unSPT $ f a) s'
                            else return $ (mzeroError, s')

instance (Monad m, Functor m) => Applicative (StatePlusT s m) where
    pure = return
    (<*>) = ap

instance Monad m => MonadState s (StatePlusT s m) where
    get = (MkSPT . StateT) $ \s -> return (snd s, s)
    put v = (MkSPT . StateT) $ \s -> return ((), (fst s, v))

plusStates :: (a, (Bool, s)) -> (a, (Bool, s)) -> (a, (Bool, s))
plusStates (_, (False, _)) b = b
plusStates a _ = a

instance Monad m => MonadPlus (StatePlusT s m) where
    mzero = (MkSPT . StateT) $ \s -> return (mzeroError, (False, snd s))
    mplus a b = (MkSPT . StateT) go where
        go s = do
            as <- runStateT (unSPT a) s
            bs <- runStateT (unSPT b) s
            let (rr, rs) = plusStates as bs
            runStateT (return rr) rs

instance (Monad m, Functor m) => Alternative (StatePlusT s m) where
    empty = mzero
    (<|>) = mplus

-- | Evaluate StatePlusT monad. In difference from runStateT it returns
-- @Nothing@ if @mzero@ has been encountered. @Just a@ otherwise.
runStatePlusT :: Monad m => StatePlusT s m a -> s -> m (Maybe a, s)
runStatePlusT sm s = do
    (v, (isOK, ss)) <- runStateT (unSPT sm) (True, s)
    return (if isOK then Just v else Nothing, ss)

-- | Execute StatePlusT monad returning resulting state
execStatePlusT :: Monad m => StatePlusT s m a -> s -> m s
execStatePlusT sm s = do
    (_, ss) <- runStatePlusT sm s
    return ss

-- | Evaluate StatePlusT monad returning resulting value. See above
-- for the semantics.
evalStatePlusT :: Monad m => StatePlusT s m a -> s -> m (Maybe a)
evalStatePlusT sm s = do
    (mb, _) <- runStatePlusT sm s
    return mb
