module System.Console.Terminfo.Edit where

import System.Console.Terminfo.Base

clear :: Terminal -> Maybe (LinesAffected -> TermOutput)
clear = fmap ($ []) . tiGetOutput "clear" 

