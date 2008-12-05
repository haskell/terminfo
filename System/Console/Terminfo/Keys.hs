-- |
-- Maintainer  : judah.jacobson@gmail.com
-- Stability   : experimental
-- Portability : portable (FFI)
--
-- The string capabilities in this module are the character sequences
-- corresponding to user input such as arrow keys and function keys.
module System.Console.Terminfo.Keys(
                    -- * The keypad
                    -- | The following commands
                    -- turn the keypad on\/off (@smkx@ and @rmkx@).  
                    -- They have no effect if those capabilities are not defined.  
                    -- For portability between terminals, the keypad should be
                    -- explicitly turned on before accepting user key input.
                    keypadOn,
                    keypadOff,
                    -- * Arrow keys
                    keyUp,
                    keyDown,
                    keyLeft,
                    keyRight,
                    -- * Miscellaneous
                    functionKey,
                    keyBackspace,
                    keyDeleteChar,
                    keyHome,
                    keyEnd
                    ) where

import System.Console.Terminfo.Base

keypadOn :: Capability TermOutput
keypadOn = tiGetOutput1 "smkx"

keypadOff :: Capability TermOutput
keypadOff = tiGetOutput1 "rmkx"

keyUp :: Capability String
keyUp = tiGetStr "kcuu1"

keyDown :: Capability String
keyDown = tiGetStr "kcud1"

keyLeft :: Capability String
keyLeft = tiGetStr "kcub1"

keyRight :: Capability String
keyRight = tiGetStr "kcuf1"

-- | Look up the control sequence for a given function sequence.  For example, 
-- @functionKey 12@ retrieves the @kf12@ capability.
functionKey :: Int -> Capability String
functionKey n = tiGetStr ("kf" ++ show n)

keyBackspace :: Capability String
keyBackspace = tiGetStr "kbs"

keyDeleteChar :: Capability String
keyDeleteChar = tiGetStr "kdch1"

keyHome :: Capability String
keyHome = tiGetStr "khome"

keyEnd :: Capability String
keyEnd = tiGetStr "kend"
