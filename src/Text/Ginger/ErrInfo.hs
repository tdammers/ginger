{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Text.Ginger.ErrInfo
    ( ErrInfo (..)
    ) where

{-| A newtype around 'Either' that lets us define a useful 'MonadFail' instance. -}
newtype ErrInfo a = ErrInfo { runErrInfo :: Either String a } deriving (Functor, Applicative, Monad)

instance MonadFail ErrInfo where
    fail = ErrInfo . Left
