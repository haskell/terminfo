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

withStandout :: Capability (TermOutput -> TermOutput)
withStandout = wrapWith enterStandoutMode exitStandoutMode
withUnderline :: Capability (TermOutput -> TermOutput)
withUnderline = wrapWith enterUnderlineMode exitUnderlineMode

enterStandoutMode :: Capability TermOutput
enterStandoutMode = tiGetOutput1 "smso"

exitStandoutMode :: Capability TermOutput
exitStandoutMode = tiGetOutput1 "rmso"

enterUnderlineMode :: Capability TermOutput
enterUnderlineMode = tiGetOutput1 "smul"

exitUnderlineMode :: Capability TermOutput
exitUnderlineMode = tiGetOutput1 "rmul"



bell :: Capability TermOutput
bell = tiGetOutput1 "bel"

visualBell :: Capability TermOutput
visualBell = tiGetOutput1 "flash"
