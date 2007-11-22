module System.Console.Terminfo.Param where

import System.Console.Terminfo.Base

class TOut a where
    tOut :: [Int] -> a

runTOut :: TOut a => a
runTOut = tOut []

instance TOut (LinesAffected -> TermOutput) where
instance TOut TermOutput where

instance (Enum a, TOut f) => TOut (a -> f) where


-- only concrete function exported:
tiGetOutput :: TOut f => String -> Terminal -> IO (Maybe f)
