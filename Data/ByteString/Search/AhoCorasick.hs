{-# LANGUAGE ForeignFunctionInterface, BangPatterns #-}

module Data.ByteString.Search.AhoCorasick
    (Dictionary(), Match(..), Span, buildDictionary, search)
    where

import Control.Monad
import Data.ByteString (ByteString)
import Foreign hiding (unsafePerformIO)
import Foreign.Ptr
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Utils
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.ByteString as BS

type ACSTRUCTPtr = Ptr ()
type ACSTRUCTFPtr = ForeignPtr ()

-- The better solution for scalability would be copying the structures on search instead of mutual exclusion, but that's for another time.
newtype Dictionary = Dictionary ACSTRUCTFPtr

data Match = Match ByteString Span deriving (Show, Eq)

-- | The index of the first character matched and the index of one past the last character matched
type Span = (Int, Int)

foreign import ccall "ac.h ac_alloc"
    ac_alloc :: IO ACSTRUCTPtr

foreign import ccall "ac.h ac_add_string"
    ac_add_string :: ACSTRUCTPtr -> CString -> CInt -> CInt -> IO CInt

foreign import ccall "ac.h ac_prep"
    ac_prep :: ACSTRUCTPtr -> IO CInt

foreign import ccall "ac.h ac_search_init"
    ac_search_init :: ACSTRUCTPtr -> CString -> CInt -> IO ()

foreign import ccall "ac.h ac_search"
    ac_search :: ACSTRUCTPtr -> Ptr CInt -> Ptr CInt -> Ptr CInt
              -> IO (Ptr CChar)

foreign import ccall "ac.h &ac_free"
    ac_free :: FunPtr (ACSTRUCTPtr -> IO ())

foreign import ccall "ac.h ac_shallow_cpy"
    ac_shallow_cpy :: ACSTRUCTPtr -> IO (ACSTRUCTPtr)

foreign import ccall "ac.h ac_cpy_free"
    ac_cpy_free :: ACSTRUCTPtr -> IO ()

-- Would be good to check return codes to ensure that everything succeeds
-- | Constructs a dictionary as a set of strings for matching
buildDictionary :: [ByteString] -> Dictionary
buildDictionary strings = unsafePerformIO $ ac_alloc >>= \(!node) -> do
    forM_ stringsWithIDs $ \(strID, s) -> BS.useAsCStringLen s $
        \(s', len) -> ac_add_string node s' (fromIntegral len) strID
    _ <- ac_prep node
    nodeFP <- newForeignPtr ac_free node
    return $ Dictionary nodeFP
    where stringsWithIDs = zip [1..] strings


-- | Searches a string for matches existing in the given dictionary
search :: Dictionary -> ByteString -> [Match]
search (Dictionary nodeFP) target = unsafePerformIO $
        withForeignPtr nodeFP $ \node -> do
        node_cpy <- ac_shallow_cpy node
        BS.useAsCStringLen target $ \(t', tLen) ->
            ac_search_init node_cpy t' (fromIntegral tLen)
        !ms <- buildMatchList node_cpy []
        ac_cpy_free node_cpy
        return ms
    where
        buildMatchList node matches =
            alloca $ \matchStartPtr ->
            alloca $ \matchLenPtr ->
            alloca $ \matchIDPtr -> do
                ccharPtr <- ac_search node matchStartPtr matchLenPtr
                    matchIDPtr
                mcchar <- maybePeek peek ccharPtr
                case mcchar of
                    Just cchar -> do
                        len <- fmap fromIntegral $ peek matchLenPtr
                        matchStr <- BS.packCStringLen (ccharPtr, len)
                        matchStart <- fmap fromIntegral $ peek matchStartPtr
                        let match = Match matchStr
                                (matchStart, matchStart + len)
                        buildMatchList node (match:matches)
                    Nothing -> return $ reverse matches
