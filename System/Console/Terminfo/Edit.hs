module System.Console.Terminfo.Edit where

import System.Console.Terminfo.Base

-- | Clear the screen, and move the cursor to the upper left.
clearScreen :: Terminal -> Maybe (LinesAffected -> TermOutput)
clearScreen = fmap ($ []) . tiGetOutput "clear" 

-- | Clear from beginning of line to cursor.
clearBOL :: Terminal -> Maybe TermOutput
clearBOL = tiGetOutput1 "el1"

-- | Clear from cursor to end of line.
clearEOL :: Terminal -> Maybe TermOutput
clearEOL = tiGetOutput1 "el"

-- | Clear display after cursor.
clearEOS :: Terminal -> Maybe (LinesAffected -> TermOutput)
clearEOS = fmap ($ []) . tiGetOutput "ed"

