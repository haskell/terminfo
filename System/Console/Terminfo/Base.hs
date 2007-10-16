-- | This module provides a low-level interface to the C functions of the 
-- terminfo library. 
module System.Console.Terminfo.Base(
                            Terminal(),
                            setupTerm,
                            setupTermFromEnv,
                            tiGetFlag,
                            tiGetNum,
                            tiGetStr,
                            tPuts,
                            LinesAffected,
                            tParm,
                            TParm(..),
                            TParmInput
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

data TERMINAL = TERMINAL
newtype Terminal = Terminal (ForeignPtr TERMINAL)

foreign import ccall "&" cur_term :: Ptr (Ptr TERMINAL)
foreign import ccall set_curterm :: Ptr TERMINAL -> IO (Ptr TERMINAL)
foreign import ccall "&" del_curterm :: FunPtr (Ptr TERMINAL -> IO ())

-- TODO: Explicit header import.
foreign import ccall setupterm :: CString -> CInt -> Ptr CInt -> IO ()

-- | Initialize the terminfo library to the given terminal entry.
--
-- Note: Either 'setupTerm' or 'setupTermFromEnv' must be run before any capabilities
-- are looked up.
setupTerm :: String -> IO Terminal
setupTerm term = withCString term $ \c_term -> 
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

withCurTerm :: (CString -> IO a) -> Terminal -> String -> a
withCurTerm f (Terminal term) cap = unsafePerformIO $ withMVar cursesLock $ \_ -> 
    withForeignPtr term set_curterm >> withCString cap f


foreign import ccall tigetnum :: CString -> IO CInt

-- | Look up a numeric capability in the terminfo database.
tiGetNum :: Terminal -> String -> Maybe Int
tiGetNum = withCurTerm $ \c_cap -> do
                n <- fmap fromEnum (tigetnum c_cap)
                if n >= 0
                    then return (Just n)
                    else return Nothing

foreign import ccall tigetflag :: CString -> IO CInt
-- | Look up a boolean capability in the terminfo database.
tiGetFlag :: Terminal -> String -> Bool
tiGetFlag = withCurTerm $ \c_cap -> do
                fmap (>0) (tigetflag c_cap)
                
                
foreign import ccall tigetstr :: CString -> IO CString

-- | Look up a string capability in the terminfo database.  
--
-- Note: All terminfo strings should be printed with 'tPuts'.
tiGetStr :: Terminal -> String -> Maybe String
tiGetStr = withCurTerm $ \c_cap -> do
                result <- tigetstr c_cap
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

type TParmInput = (Int, Int, Int, Int, Int, Int, Int, Int, Int)

class TParm a where
    toTParmInput :: a -> TParmInput

instance TParm TParmInput where
    toTParmInput = id

instance TParm Int where
    toTParmInput p1 = (p1,0,0,0,0,0,0,0,0)

instance TParm (Int,Int) where
    toTParmInput (p1,p2) = (p1,p2,0,0,0,0,0,0,0)

instance TParm (Int,Int,Int) where
    toTParmInput (p1,p2,p3) = (p1,p2,p3,0,0,0,0,0,0)

instance TParm (Int,Int,Int,Int) where
    toTParmInput (p1,p2,p3,p4) = (p1,p2,p3,p4,0,0,0,0,0)

instance TParm (Int,Int,Int,Int,Int) where
    toTParmInput (p1,p2,p3,p4,p5) = (p1,p2,p3,p4,p5,0,0,0,0)

instance TParm (Int,Int,Int,Int,Int,Int) where
    toTParmInput (p1,p2,p3,p4,p5,p6) = (p1,p2,p3,p4,p5,p6,0,0,0)

instance TParm (Int,Int,Int,Int,Int,Int,Int) where
    toTParmInput (p1,p2,p3,p4,p5,p6,p7) = (p1,p2,p3,p4,p5,p6,p7,0,0)

instance TParm (Int,Int,Int,Int,Int,Int,Int,Int) where
    toTParmInput (p1,p2,p3,p4,p5,p6,p7,p8) = (p1,p2,p3,p4,p5,p6,p7,p8,0)


-- Note: the purity of this function really depends on the implementation;
-- and this may not be multithreaded-safe.
tparm_base :: String -> TParmInput -> String
tparm_base cap (p1,p2,p3,p4,p5,p6,p7,p8,p9)
    = unsafePerformIO $ withCString cap $ \c_cap -> do
        result <- tparm c_cap (toEnum p1) (toEnum p2) (toEnum p3) (toEnum p4) 
                    (toEnum p5) (toEnum p6) (toEnum p7) (toEnum p8) (toEnum p9)
        peekCString result

-- | Substitute parameters into a string capability.
--
-- Note: All terminfo strings should be printed with 'tPuts'.
tParm :: TParm a => String -> a -> String
tParm s x = tparm_base s (toTParmInput x)



type CharOutput = CInt -> IO CInt
foreign import ccall "wrapper" mkCallback :: CharOutput -> IO (FunPtr CharOutput)

foreign import ccall tputs :: CString -> CInt -> FunPtr CharOutput -> IO ()

type LinesAffected = Int

-- | Output a string capability.  Applys padding information to the string if
-- necessary.
tPuts :: String -> LinesAffected 
        -> (Char -> IO ()) -- ^ An output function, e.g. 'putChar'.
        -> IO ()
tPuts s n f = withCString s $ \c_str -> do
                fun <- mkCallback (\c -> let c' = toEnum $ fromEnum c 
                                         in f c' >> return c)
                tputs c_str (toEnum n) fun
                freeHaskellFunPtr fun

