-- |
-- Maintainer  : judah.jacobson@gmail.com
-- Stability   : experimental
-- Portability : portable (FFI)
module System.Console.Terminfo.Effects(
                    -- * Bell alerts
                    bell,visualBell,
                    -- * Text effect wrappers
                    withStandout,
                    withUnderline,
                    withBold,
                    -- * Modes
                    enterStandoutMode,
                    exitStandoutMode,
                    enterUnderlineMode,
                    exitUnderlineMode,
                    -- * Attributes
                    boldOn,
                    allAttributesOff
                    ) where

import System.Console.Terminfo.Base

wrapWith :: Capability TermOutput -> Capability TermOutput 
                -> Capability (TermOutput -> TermOutput)
wrapWith start end = do
    s <- start
    e <- end
    return (\t -> s <#> t <#> e)

-- | Turns on standout mode before outputting the given
-- text, and then turns it off.
withStandout :: Capability (TermOutput -> TermOutput)
withStandout = wrapWith enterStandoutMode exitStandoutMode

-- | Turns on underline mode before outputting the given
-- text, and then turns it off.
withUnderline :: Capability (TermOutput -> TermOutput)
withUnderline = wrapWith enterUnderlineMode exitUnderlineMode

-- | Turns on bold mode before outputting the given text, and then turns
-- all attributes off.
withBold :: Capability (TermOutput -> TermOutput)
withBold = wrapWith boldOn allAttributesOff

enterStandoutMode :: Capability TermOutput
enterStandoutMode = tiGetOutput1 "smso"

exitStandoutMode :: Capability TermOutput
exitStandoutMode = tiGetOutput1 "rmso"

enterUnderlineMode :: Capability TermOutput
enterUnderlineMode = tiGetOutput1 "smul"

exitUnderlineMode :: Capability TermOutput
exitUnderlineMode = tiGetOutput1 "rmul"

boldOn :: Capability TermOutput
boldOn = tiGetOutput1 "bold"

allAttributesOff :: Capability TermOutput
allAttributesOff = tiGetOutput1 "sgr0"

-- | Sound the audible bell.
bell :: Capability TermOutput
bell = tiGetOutput1 "bel"

-- | Present a visual alert using the @flash@ capability.
visualBell :: Capability TermOutput
visualBell = tiGetOutput1 "flash"
