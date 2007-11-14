-- | This module provides a low-level interface to the C functions of the 
-- terminfo library. 
module System.Console.Terminfo.Base(
                            Terminal(),
                            setupTerm,
                            setupTermFromEnv,
                            tiGetFlag,
                            tiGetNum,
                            tiGetStr,
                            LinesAffected,
                            TermOutput(),
                            tiGetOutput,
                            runTermOutput,
                            termOutput
                            ) where

import Foreign.C
import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Marshal
import Foreign.Storable
import System.Environment
import Control.Monad
import System.IO.Unsafe
import Control.Concurrent.MVar

import Data.Monoid

data TERMINAL = TERMINAL
newtype Terminal = Terminal (ForeignPtr TERMINAL)

foreign import ccall "&" cur_term :: Ptr (Ptr TERMINAL)
foreign import ccall set_curterm :: Ptr TERMINAL -> IO (Ptr TERMINAL)
foreign import ccall "&" del_curterm :: FunPtr (Ptr TERMINAL -> IO ())

-- TODO: Explicit header import.
foreign import ccall setupterm :: CString -> CInt -> Ptr CInt -> IO ()

-- | Initialize the terminfo library to the given terminal entry.
--
setupTerm :: String -> IO Terminal
setupTerm term = withCursesLock $ withCString term $ \c_term -> 
                    with 0 $ \ret_ptr -> do
                        let stdOutput = 1
                        setupterm c_term stdOutput ret_ptr
                        ret <- peek ret_ptr
                        when (ret /= 1) $ error ("Couldn't lookup terminfo entry " ++ show term)
                        cterm <- peek cur_term
                        fmap Terminal $ newForeignPtr del_curterm cterm
                            
-- | Initialize the terminfo library, using the @TERM@ environmental variable.
-- If @TERM@ is not set, we use the generic, minimal entry @dumb@.
setupTermFromEnv :: IO Terminal
setupTermFromEnv = do
    env_term <- getEnv "TERM" 
    let term = if null env_term then "dumb" else env_term
    setupTerm term

{-- TODO: maybe???
data Available a = WrongType | Absent | Present a
flag : ()
class Capability a where
    getCap :: String -> Int (Available a)
--}

-- ncurses is extremely unsafe for multithreaded calls.
{-# NOINLINE cursesLock #-}
cursesLock :: MVar TERMINAL
cursesLock = unsafePerformIO $ newMVar TERMINAL

withCursesLock :: IO a -> IO a
withCursesLock f = withMVar cursesLock $ \_ -> f


withCurTerm :: Terminal -> IO a -> IO a
withCurTerm (Terminal term) f = withCursesLock $ do
    withForeignPtr term set_curterm 
    f


foreign import ccall tigetnum :: CString -> IO CInt

-- | Look up a numeric capability in the terminfo database.
tiGetNum :: String -> Terminal -> IO (Maybe Int)
tiGetNum cap term = withCurTerm term $ do
                n <- fmap fromEnum (withCString cap tigetnum)
                if n >= 0
                    then return (Just n)
                    else return Nothing

foreign import ccall tigetflag :: CString -> IO CInt
-- | Look up a boolean capability in the terminfo database.
tiGetFlag :: String -> Terminal -> IO Bool
tiGetFlag cap term = withCurTerm term $ 
                fmap (>0) (withCString cap tigetflag)
                
                
foreign import ccall tigetstr :: CString -> IO CString

-- | Look up a string capability in the terminfo database.  
--
-- Note: All terminfo strings should be printed with 'tPuts'.
tiGetStr :: String -> Terminal -> IO (Maybe String)
tiGetStr cap term = withCurTerm term $ do
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

tiGetOutput :: String -> Terminal -> IO (Maybe 
                ([Int] -> LinesAffected -> TermOutput))
tiGetOutput cap term = do
    mstr <- tiGetStr cap term
    return $ flip fmap mstr $ \str ps la -> TermOutput $ do
        outStr <- tParm str ps
        tPuts outStr la




foreign import ccall tputs :: CString -> CInt -> FunPtr (CInt -> IO CInt) -> IO ()
foreign import ccall "&" putchar :: FunPtr (CInt -> IO CInt)

type LinesAffected = Int

-- | Output a string capability.  Applys padding information to the string if
-- necessary.
tPuts :: String -> LinesAffected -> IO ()
tPuts s n = withCString s $ \c_str -> do
                tputs c_str (toEnum n) putchar

newtype TermOutput = TermOutput (IO ())

runTermOutput :: Terminal -> TermOutput -> IO ()
runTermOutput term (TermOutput to) = withCurTerm term to

termOutput :: String -> TermOutput
termOutput = TermOutput . putStr

instance Monoid TermOutput where 
    mempty = TermOutput $ return ()
    TermOutput f `mappend` TermOutput g = TermOutput (f >> g) 
