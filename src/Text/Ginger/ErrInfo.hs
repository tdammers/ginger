{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Text.Ginger.ErrInfo
    ( ErrInfo (..)
    ) where

newtype ErrInfo a = ErrInfo { runErrInfo :: Either String a } deriving (Functor, Applicative, Monad)

instance MonadFail ErrInfo where
    fail = ErrInfo . Left