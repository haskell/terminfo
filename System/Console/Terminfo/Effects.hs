-- |
-- Maintainer  : judah.jacobson@gmail.com
-- Stability   : experimental
-- Portability : portable (FFI)
module System.Console.Terminfo.Effects(
                    -- * Bell alerts
                    bell,visualBell,
                    -- * text effects
                    withStandout,withUnderline,
                    enterStandoutMode,
                    exitStandoutMode,
                    enterUnderlineMode,
                    exitUnderlineMode
                    ) where

import System.Console.Terminfo.Base

wrapWith :: Capability TermOutput -> Capability TermOutput 
                -> Capability (TermOutput -> TermOutput)
wrapWith start end = do
    s <- start
    e <- end
    return (\t -> s <#> t <#> e)

withStandout, withUnderline :: Capability (TermOutput -> TermOutput)
withStandout = wrapWith enterStandoutMode exitStandoutMode
withUnderline = wrapWith enterUnderlineMode exitUnderlineMode

enterStandoutMode, exitStandoutMode :: Capability TermOutput
enterStandoutMode = tiGetOutput1 "smso"
exitStandoutMode = tiGetOutput1 "rmso"

enterUnderlineMode, exitUnderlineMode :: Capability TermOutput
enterUnderlineMode = tiGetOutput1 "smul"
exitUnderlineMode = tiGetOutput1 "rmul"


bell, visualBell :: Capability TermOutput
bell = tiGetOutput1 "bel"
visualBell = tiGetOutput1 "flash"
