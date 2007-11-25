module System.Console.Terminfo.Cursor where

import System.Console.Terminfo.Base
import Control.Monad

lines, columns :: Capability Int
lines = tiGetNum "lines"
columns = tiGetNum "columns"

{--
On many terminals, the @cud1@ ('cursorDown1') capability is the line feed 
character '\n'.  However, @stty@ settings may cause that character to have
other effects than intended; e.g. ONLCR turns LF into CRLF, and as a result 
@cud1@ will always move the cursor to the first column of the next line.  

Looking at the source code of curses (lib_mvcur.c) and other similar programs, 
they use @cud@ instead of @cud1@ if it's '\n' and ONLCR is turned on.  

Since there's no easy way to check for ONLCR at this point, I've just made
cursorDown1 always use @cud@.

Note the same problems apply to @ind@, but I think there's less of an
expectation that scrolling down will keep the same column.  
Suggestions are welcome.
--}
cursorDown1, cursorLeft1,cursorRight1,cursorUp1 :: Capability TermOutput
cursorDown1 = fmap ($1) cursorDown
cursorLeft1 = tiGetOutput1 "cub1"
cursorRight1 = tiGetOutput1 "cuf1"
cursorUp1 = tiGetOutput1 "cuu1"

cursorDown, cursorLeft, cursorRight, cursorUp :: Capability (Int -> TermOutput)
cursorDown = tiGetOutput1 "cud"
cursorLeft = tiGetOutput1 "cub"
cursorRight = tiGetOutput1 "cuf"
cursorUp = tiGetOutput1 "cuu"

cursorHome, cursorToLL :: Capability TermOutput
cursorHome = tiGetOutput1 "home"
cursorToLL = tiGetOutput1 "ll"
-- | The @cr@ capability, which moves the cursor to the first column of the
-- current line.
carriageReturn :: Capability TermOutput
carriageReturn = tiGetOutput1 "cr"

-- | The @nel@ capability, which moves the cursor to the first column of
-- the next line.  It behaves like a carriage return followed by a line feed.
--
-- If @nel@ is not defined, this may be built out of other capabilities.
newline :: Capability TermOutput
newline = tiGetOutput1 "nel" 
    `mplus` (liftM2 mappend carriageReturn 
                            (scrollForward `mplus` tiGetOutput1 "cud1"))
        -- Note it's OK to use cud1 here, despite the stty problem referenced 
        -- above, because carriageReturn already puts us on the first column.

scrollForward, scrollReverse :: Capability TermOutput
scrollForward = tiGetOutput1 "ind"
scrollReverse = tiGetOutput1 "ri"


data Point = Point {row, col :: Int}

cursorAddress :: Capability (Point -> TermOutput)
cursorAddress = fmap (\g p -> g (row p) (col p)) $ tiGetOutput1 "cup"

columnAddress, rowAddress :: Capability (Int -> TermOutput)
columnAddress = tiGetOutput1 "hpa"
rowAddress = tiGetOutput1 "vpa"


