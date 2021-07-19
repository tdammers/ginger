{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
module Text.PrintfA
where

import Text.Printf

data PrintfArgT = forall a. PrintfArg a => P a
data PrintfTypeT = T { unT :: forall r. PrintfType r => r }

printfa :: PrintfType t => String -> [ PrintfArgT ] -> t
printfa format = (\(T t) -> t) . foldl (\(T r) (P a) -> T $ r a ) (T $ printf format)
