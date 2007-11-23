{-| The string parameters in this module are the character sequences
corresponding to user input such as arrow keys and function keys.
-}
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
                    functionKey
                    ) where

import System.Console.Terminfo.Base
import Data.Maybe

keypadOn, keypadOff :: Terminal -> TermOutput
keypadOn = orDoNothing . tiGetOutput1 "smkx"
keypadOff = orDoNothing . tiGetOutput1 "rmkx"

orDoNothing :: Monoid a => Maybe a -> a
orDoNothing = fromMaybe mempty

keyUp, keyDown, keyLeft, keyRight :: Terminal -> Maybe String
keyUp = tiGetStr "kcuu1"
keyDown = tiGetStr "kcud1"
keyLeft = tiGetStr "kcub1"
keyRight = tiGetStr "kcur1"

-- | Look up the control sequence for a given function sequence.  For example, 
-- @functionKey 12@ retrieves the @kf12@ capability.
functionKey :: Int -> Terminal -> Maybe String
functionKey n = tiGetStr ("kf" ++ show n)

