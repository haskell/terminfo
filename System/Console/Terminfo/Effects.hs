-- |
-- Maintainer  : judah.jacobson@gmail.com
-- Stability   : experimental
-- Portability : portable (FFI)
module System.Console.Terminfo.Effects(
                    -- * Bell alerts
                    bell,visualBell,
                    -- * Text attributes
                    Attributes(..),
                    defaultAttributes,
                    withAttributes,
                    setAttributes,
                    allAttributesOff,
                    -- ** Mode wrappers
                    withStandout,
                    withUnderline,
                    withBold,
                    -- ** Low-level capabilities
                    enterStandoutMode,
                    exitStandoutMode,
                    enterUnderlineMode,
                    exitUnderlineMode,
                    reverseOn,
                    blinkOn,
                    boldOn,
                    dimOn,
                    invisibleOn,
                    protectedOn
                    ) where

import System.Console.Terminfo.Base
import Control.Monad

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

reverseOn :: Capability TermOutput
reverseOn = tiGetOutput1 "rev"

blinkOn:: Capability TermOutput
blinkOn = tiGetOutput1 "blink"

boldOn :: Capability TermOutput
boldOn = tiGetOutput1 "bold"

dimOn :: Capability TermOutput
dimOn = tiGetOutput1 "dim"

invisibleOn :: Capability TermOutput
invisibleOn = tiGetOutput1 "invis"

protectedOn :: Capability TermOutput
protectedOn = tiGetOutput1 "prot"

-- | Turns off all text attributes.  This capability will always succeed, but it has
-- no effect in terminals which do not support text attributes.
allAttributesOff :: Capability TermOutput
allAttributesOff = tiGetOutput1 "sgr0" `mplus` return mempty

data Attributes = Attributes {
                    standoutAttr,
                    underlineAttr,
                    reverseAttr,
                    blinkAttr,
                    dimAttr,
                    boldAttr,
                    invisibleAttr,
                    protectedAttr :: Bool
                -- NB: I'm not including the "alternate character set." 
                }

-- | Sets the attributes on or off before outputting the given text,
-- and then turns them all off.  This capability will always succeed; properties
-- which cannot be set in the current terminal will be ignored.
withAttributes :: Capability (Attributes -> TermOutput -> TermOutput)
withAttributes = do
    set <- setAttributes
    off <- allAttributesOff
    return $ \attrs to -> set attrs <#> to <#> off

-- | Sets the attributes on or off.  This capability will always succeed;
-- properties which cannot be set in the current terminal will be ignored.
setAttributes :: Capability (Attributes -> TermOutput)
setAttributes = usingSGR0 `mplus` manualSets
    where
        usingSGR0 = do
            sgr <- tiGetOutput "sgr"
            return $ \a -> let mkAttr f = if f a then 1 else 0
                           in flip sgr 0 $ map mkAttr [ standoutAttr
                                                       , underlineAttr
                                                       , reverseAttr
                                                       , blinkAttr
                                                       , dimAttr
                                                       , boldAttr
                                                       , invisibleAttr
                                                       , protectedAttr
                                                       ]
                                                ++ [0] -- for alt. char sets
        attrCap :: (Attributes -> Bool) -> Capability TermOutput
                    -> Capability (Attributes -> TermOutput)
        attrCap f cap = do {to <- cap; return $ \a -> if f a then to else mempty}
                        `mplus` return (const mempty)
        manualSets = do
            allOff <- allAttributesOff
            cs <- sequence [attrCap standoutAttr enterStandoutMode
                            , attrCap underlineAttr enterUnderlineMode
                            , attrCap reverseAttr reverseOn
                            , attrCap blinkAttr blinkOn
                            , attrCap boldAttr boldOn
                            , attrCap dimAttr dimOn
                            , attrCap invisibleAttr invisibleOn
                            , attrCap protectedAttr protectedOn
                            ]
            return $ \a -> mconcat $ map ($ a) cs

                                     

-- | These attributes have all properties turned off.
defaultAttributes :: Attributes
defaultAttributes = Attributes False False False False False False False False

-- | Sound the audible bell.
bell :: Capability TermOutput
bell = tiGetOutput1 "bel"

-- | Present a visual alert using the @flash@ capability.
visualBell :: Capability TermOutput
visualBell = tiGetOutput1 "flash"
