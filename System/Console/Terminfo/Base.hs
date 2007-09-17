-- | This module provides a low-level interface with the C functions of the 
-- terminfo library. 
module System.Console.Terminfo.Base(
                            setupTerm,
                            setupTermFromEnv,
                            tiGetFlag,
                            tiGetNum,
                            tiGetStr,
                            tPuts,
                            tParm,
                            TParm(..),
                            ) where

import Foreign.C
import Foreign.Ptr
import Foreign.Marshal
import Foreign.Storable
import System.Environment
import Control.Monad
import System.IO.Unsafe

-- TODO: Explicit header import.
foreign import ccall setupterm :: CString -> CInt -> Ptr CInt -> IO ()

-- Initialize the terminfo library to the given terminal entry, 
-- using the system terminfo directory.  
setupTerm :: String -> IO ()
setupTerm term = withCString term $ \c_term -> 
                    with 0 $ \ret_ptr -> do
                        let stdOutput = 1
                        setupterm c_term stdOutput ret_ptr
                        ret <- peek ret_ptr
                        when (ret /= 1) $ error ("Couldn't lookup terminfo entry " ++ show term)
                            
-- Initialize the terminfo library, using the TERM environmental variable.
-- If TERM is not set, we use the generic entry "dumb".
setupTermFromEnv :: IO ()
setupTermFromEnv = do
    env_term <- getEnv "TERM" 
    let term = if null env_term then "dumb" else env_term
    setupTerm term

foreign import ccall tparm ::
    CString -> CLong -> CLong -> CLong -> CLong -> CLong -> CLong 
    -> CLong -> CLong -> CLong -- p1,...,p9
    -> IO CString


type TParmFunc = 
    Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> String

class TParm f where
    applyTParm :: TParmFunc -> f

instance TParm (Int -> String) where
    applyTParm f p1 = f p1 0 0 0 0 0 0 0 0

instance TParm (Int -> Int -> String) where
    applyTParm f p1 p2 = f p1 p2 0 0 0 0 0 0 0

-- Note: the purity of this function really depends on the implementation;
-- and this may not be multithreaded-safe.
tparm_base :: String -> TParmFunc
tparm_base cap p1 p2 p3 p4 p5 p6 p7 p8 p9 
    = unsafePerformIO $ withCString cap $ \c_cap -> do
        result <- tparm c_cap (toEnum p1) (toEnum p2) (toEnum p3) (toEnum p4) 
                    (toEnum p5) (toEnum p6) (toEnum p7) (toEnum p8) (toEnum p9)
        peekCString result

tParm :: TParm f => String -> f
tParm s = applyTParm (tparm_base s)





foreign import ccall tigetnum :: CString -> IO CInt
tiGetNum :: String -> IO (Maybe Int)
tiGetNum cap = withCString cap $ \c_cap -> do
                n <- fmap fromEnum (tigetnum c_cap)
                if n >= 0
                    then return (Just n)
                    else return Nothing

                
-- TODO: what if not available
foreign import ccall tigetstr :: CString -> IO CString

tiGetStr :: String -> IO (Maybe String)
tiGetStr cap = withCString cap $ \c_cap -> do
                result <- tigetstr c_cap
                if result == nullPtr || result == neg1Ptr
                    then return Nothing
                    else fmap Just (peekCString result)

-- hack; tigetstr sometimes returns (-1)
neg1Ptr = nullPtr `plusPtr` (-1)
                    
foreign import ccall tigetflag :: CString -> IO CInt
tiGetFlag :: String -> IO Bool
tiGetFlag cap = withCString cap $ \c_cap -> 
                fmap (== 1) (tigetflag c_cap)
                
-- TODO: initialize PC to "pc" capability, or null otherwise.
-- also: PC, ospeed, UP, BC
-- Note: I may want to cut out the middleman and pipe tGoto/tGetStr together
-- with tput without a String marshall in the middle.
-- directly without 

type CharOutput = CInt -> IO CInt
foreign import ccall "wrapper" mkCallback :: CharOutput -> IO (FunPtr CharOutput)

foreign import ccall tputs :: CString -> CInt -> FunPtr CharOutput -> IO ()

tPuts :: String -> Int -> (Char -> IO ()) -> IO ()
tPuts s n f = withCString s $ \c_str -> do
                fun <- mkCallback (\c -> let c' = toEnum $ fromEnum c 
                                         in f c' >> return c)
                tputs c_str (toEnum n) fun
                freeHaskellFunPtr fun



