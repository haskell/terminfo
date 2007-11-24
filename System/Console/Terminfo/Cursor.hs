module System.Console.Terminfo.Cursor where

import System.Console.Terminfo.Base

lines, columns :: Capability Int
lines = tiGetNum "lines"
columns = tiGetNum "columns"

cursorDown1, cursorLeft1,cursorRight1,cursorUp1 :: Capability TermOutput
cursorDown1 = tiGetOutput1 "cud1"
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
newline :: Capability TermOutput
newline = tiGetOutput1 "nel"


data Point = Point {row, col :: Int}

cursorAddress :: Capability (Point -> TermOutput)
cursorAddress = fmap (\g p -> g (row p) (col p)) $ tiGetOutput1 "cup"

columnAddress, rowAddress :: Capability (Int -> TermOutput)
columnAddress = tiGetOutput1 "hpa"
rowAddress = tiGetOutput1 "vpa"
