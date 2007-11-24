module System.Console.Terminfo.Cursor where

import System.Console.Terminfo.Base

lines, columns :: Terminal -> Maybe Int
lines = tiGetNum "lines"
columns = tiGetNum "columns"

cursorDown1, cursorLeft1,cursorRight1,cursorUp1 :: Terminal -> Maybe TermOutput
cursorDown1 = tiGetOutput1 "cud1"
cursorLeft1 = tiGetOutput1 "cub1"
cursorRight1 = tiGetOutput1 "cuf1"
cursorUp1 = tiGetOutput1 "cuu1"

cursorDown, cursorLeft, cursorRight, cursorUp :: Terminal -> Maybe (Int -> TermOutput)
cursorDown = tiGetOutput1 "cud"
cursorLeft = tiGetOutput1 "cub"
cursorRight = tiGetOutput1 "cuf"
cursorUp = tiGetOutput1 "cuu"

cursorHome, cursorToLL :: Terminal -> Maybe TermOutput
cursorHome = tiGetOutput1 "home"
cursorToLL = tiGetOutput1 "ll"
-- | The @cr@ capability, which moves the cursor to the first column of the
-- current line.
carriageReturn :: Terminal -> Maybe TermOutput
carriageReturn = tiGetOutput1 "cr"

-- | The @nel@ capability, which moves the cursor to the first column of
-- the next line.  It behaves like a carriage return followed by a line feed.
newline :: Terminal -> Maybe TermOutput
newline = tiGetOutput1 "nel"


data Point = Point {row, col :: Int}

cursorAddress :: Terminal -> Maybe (Point -> TermOutput)
cursorAddress = fmap (\g p -> g (row p) (col p)) . tiGetOutput1 "cup"

columnAddress, rowAddress :: Terminal -> Maybe (Int -> TermOutput)
columnAddress = tiGetOutput1 "hpa"
rowAddress = tiGetOutput1 "vpa"
