{-# INCLUDE <term.h> #-}
-- |
-- Maintainer  : judah.jacobson@gmail.com
-- Stability   : experimental
-- Portability : portable (FFI)
--
-- This module provides a low-level interface to the C functions of the 
-- terminfo library. 
-- 
-- NOTE: Since this library is built on top of the curses interface, it is not thread-safe.

module System.Console.Terminfo.Base(
                            Terminal(),
                            setupTerm,
                            setupTermFromEnv,
                            Capability,
                            getCapability,
                            tiGetFlag,
                            tiGuardFlag,
                            tiGetNum,
                            tiGetStr,
                            TermOutput(),
                            runTermOutput,
                            termText,
                            tiGetOutput,
                            LinesAffected,
                            tiGetOutput1,
                            OutputCap,
                            -- ** Monoid functions
                            Monoid(..),
                            (<#>)
                            ) where


import Control.Monad
import Data.Monoid
import Foreign.C
import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Marshal
import Foreign.Storable (peek,poke)
import System.Environment (getEnv)
import System.IO.Unsafe (unsafePerformIO)


data TERMINAL = TERMINAL
newtype Terminal = Terminal (ForeignPtr TERMINAL)

foreign import ccall "&" cur_term :: Ptr (Ptr TERMINAL)
foreign import ccall set_curterm :: Ptr TERMINAL -> IO (Ptr TERMINAL)
foreign import ccall "&" del_curterm :: FunPtr (Ptr TERMINAL -> IO ())

foreign import ccall setupterm :: CString -> CInt -> Ptr CInt -> IO ()

-- | Initialize the terminfo library to the given terminal entry.
--
setupTerm :: String -> IO Terminal
setupTerm term = withCString term $ \c_term -> 
    {-- If the terminal name is the same as for the previous call to 
    setupterm, ncurses will return cur_term instead of a new struct.
    This leads to problems when calling del_curterm on a struct shared by
    more than one Terminal.  To avoid this behavior, we temporarily set the
    cur_term value to nullPtr.
    --}
                    with 0 $ \ret_ptr -> do
                        let stdOutput = 1
                        -- set cur_term to null, temporarily
                        old_term <- peek cur_term
                        poke cur_term nullPtr
                        -- call setupterm and check the return value.
                        setupterm c_term stdOutput ret_ptr
                        ret <- peek ret_ptr
                        when (ret /= 1) $ error ("Couldn't lookup terminfo entry " ++ show term)
                        -- set cur_term to its previous value
                        cterm <- peek cur_term
                        poke cur_term old_term 
                        -- create the Terminal and return it.
                        fmap Terminal $ newForeignPtr del_curterm cterm
                            
-- | Initialize the terminfo library, using the @TERM@ environmental variable.
-- If @TERM@ is not set, we use the generic, minimal entry @dumb@.
setupTermFromEnv :: IO Terminal
setupTermFromEnv = do
    env_term <- getEnv "TERM" 
    let term = if null env_term then "dumb" else env_term
    setupTerm term

withCurTerm :: Terminal -> IO a -> IO a
withCurTerm (Terminal term) f = withForeignPtr term $ \cterm -> do
        old_term <- peek cur_term
        if old_term /= cterm
            then do
                    set_curterm cterm
                    x <- f
                    set_curterm old_term
                    return x
            else f

-- | A feature or operation which a 'Terminal' may define.
newtype Capability a = Capability (Terminal -> Maybe a)

getCapability :: Terminal -> Capability a -> Maybe a
getCapability term (Capability f) = f term

-- Note that the instances for Capability of Functor, Monad and MonadPlus 
-- use the corresponding instances for Maybe.
instance Functor Capability where
    fmap f (Capability g) = Capability (fmap f . g) 

instance Monad Capability where
    return x = Capability (\_ -> Just x)
    Capability f >>= g = Capability $ \t -> f t >>= getCapability t . g
    Capability f >> Capability g = Capability $ \t -> f t >> g t

instance MonadPlus Capability where
    mzero = Capability (\_ -> Nothing)
    Capability f `mplus` Capability g = Capability (\t -> f t `mplus` g t)

foreign import ccall tigetnum :: CString -> IO CInt

-- | Look up a numeric capability in the terminfo database.
tiGetNum :: String -> Capability Int 
tiGetNum cap = Capability $ \term -> unsafePerformIO $ withCurTerm term $ do
                n <- fmap fromEnum (withCString cap tigetnum)
                if n >= 0
                    then return (Just n)
                    else return Nothing

foreign import ccall tigetflag :: CString -> IO CInt
-- | Look up a boolean capability in the terminfo database.  
-- 
-- Unlike 'tiGuardFlag', this capability never fails; it returns 'False' if the
-- capability is absent or set to false, and returns 'True' otherwise.  
-- 
tiGetFlag :: String -> Capability Bool
tiGetFlag cap = Capability $ \term -> 
                    Just $ unsafePerformIO $ withCurTerm term $ 
                        fmap (>0) (withCString cap tigetflag)
                
-- | Look up a boolean capability in the terminfo database, and fail if
-- it\'s not defined.
tiGuardFlag :: String -> Capability ()
tiGuardFlag cap = tiGetFlag cap >>= guard
                
foreign import ccall tigetstr :: CString -> IO CString

-- | Look up a string capability in the terminfo database.  
--
-- Note: Do not use this function for terminal output; use 'tiGetOutput'
-- instead.
tiGetStr :: String -> Capability String
tiGetStr cap = Capability $ \term -> unsafePerformIO $ withCurTerm term $ do
                result <- withCString cap tigetstr 
                if result == nullPtr || result == neg1Ptr
                    then return Nothing
                    else fmap Just (peekCString result)
    where
        -- hack; tigetstr sometimes returns (-1)
        neg1Ptr = nullPtr `plusPtr` (-1)
                    
foreign import ccall tparm ::
    CString -> CLong -> CLong -> CLong -> CLong -> CLong -> CLong 
    -> CLong -> CLong -> CLong -- p1,...,p9
    -> IO CString


-- Note: I may want to cut out the middleman and pipe tGoto/tGetStr together
-- with tput without a String marshall in the middle.
-- directly without 

tParm :: String -> [Int] -> IO String
tParm cap ps = tparm' (map toEnum ps ++ repeat 0)
    where tparm' (p1:p2:p3:p4:p5:p6:p7:p8:p9:_)
            = withCString cap $ \c_cap -> do
                result <- tparm c_cap p1 p2 p3 p4 p5 p6 p7 p8 p9
                peekCString result

-- | Look up an output capability in the terminfo database.  
tiGetOutput :: String -> Capability ([Int] -> LinesAffected -> TermOutput)
tiGetOutput cap = flip fmap (tiGetStr cap) $ 
    \str ps la -> TermOutput $ do
        outStr <- tParm str ps
        tPuts outStr la

type CharOutput = CInt -> IO CInt
foreign import ccall "wrapper" mkCallback :: CharOutput -> IO (FunPtr CharOutput)
c_putChar = unsafePerformIO $ mkCallback putc
    where
        putc c = let c' = toEnum $ fromEnum c
                 in putChar c' >> return c

foreign import ccall tputs :: CString -> CInt -> FunPtr CharOutput -> IO ()

-- | A parameter to specify the number of lines affected.  Some capabilities
-- (e.g., @clear@ and @dch1@) use
-- this parameter on some terminals to compute variable-length padding.
type LinesAffected = Int

-- | Output a string capability.  Applys padding information to the string if
-- necessary.
tPuts :: String -> LinesAffected -> IO ()
tPuts s n = withCString s $ \c_str -> tputs c_str (toEnum n) c_putChar

-- | An action which sends output to the terminal.  That output may mix plain text with control
-- characters and escape sequences, along with delays (called \"padding\") required by some older
-- terminals.
newtype TermOutput = TermOutput (IO ())

runTermOutput :: Terminal -> TermOutput -> IO ()
runTermOutput term (TermOutput to) = withCurTerm term to

-- | Output plain text containing no control characters or escape sequences.
termText :: String -> TermOutput
termText = TermOutput . putStr

instance Monoid TermOutput where 
    mempty = TermOutput $ return ()
    TermOutput f `mappend` TermOutput g = TermOutput (f >> g) 

-- | A type class to encapsulate capabilities which take in zero or more 
-- parameters.
class OutputCap f where
    outputCap :: ([Int] -> TermOutput) -> [Int] -> f

instance OutputCap TermOutput where
    outputCap f xs = f (reverse xs)

instance (Enum a, OutputCap f) => OutputCap (a -> f) where
    outputCap f xs = \x -> outputCap f (fromEnum x:xs)

-- | Look up an output capability which takes a fixed number of parameters
-- (for example, @Int -> Int -> TermOutput@).
-- 
-- For capabilities which may contain variable-length
-- padding, use 'tiGetOutput' instead.
tiGetOutput1 :: OutputCap f => String -> Capability f
tiGetOutput1 str = fmap (\f -> outputCap (flip f 1) []) $ tiGetOutput str

infixl 2 <#>

-- | An operator version of 'mappend'.
(<#>) :: Monoid m => m -> m -> m
(<#>) = mappend

