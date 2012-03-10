{-# LANGUAGE ForeignFunctionInterface, BangPatterns #-}

module Data.ByteString.Search.AhoCorasick where

import Control.Monad
import Data.ByteString (ByteString)
import Foreign
import Foreign.Ptr
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Utils
import qualified Data.ByteString as BS

type ACSTRUCTPtr = Ptr ()
type ACSTRUCTFPtr = ForeignPtr ()

newtype Dictionary = Dictionary ACSTRUCTFPtr

foreign import ccall "ac.h ac_alloc"
    ac_alloc :: IO ACSTRUCTPtr

foreign import ccall "ac.h ac_add_string"
    ac_add_string :: ACSTRUCTPtr -> CString -> CInt -> CInt -> IO CInt

foreign import ccall "ac.h ac_prep"
    ac_prep :: ACSTRUCTPtr -> IO CInt

foreign import ccall "ac.h ac_search_init"
    ac_search_init :: ACSTRUCTPtr -> CString -> CInt -> IO ()

foreign import ccall "ac.h ac_search"
    ac_search :: ACSTRUCTPtr -> Ptr CInt -> Ptr CInt -> IO (Ptr CChar)

foreign import ccall "ac.h &ac_free"
    ac_free :: FunPtr (ACSTRUCTPtr -> IO ())

-- Would be good to check return codes to ensure that everything succeeds
-- | Constructs a dictionary as a set of strings for matching
buildDictionary :: [ByteString] -> IO Dictionary
buildDictionary strings = ac_alloc >>= \(!node) -> do
    forM_ stringsWithIDs $ \(strID, s) -> BS.useAsCStringLen s $
        \(s', len) -> ac_add_string node s' (fromIntegral len) strID
    _ <- ac_prep node
    nodeFP <- newForeignPtr ac_free node
    return $ Dictionary nodeFP
    where stringsWithIDs = zip [1..] strings


-- | Searches a string for matches existing in the given dictionary
search :: Dictionary -> ByteString -> IO [ByteString]
search (Dictionary nodeFP) target = withForeignPtr nodeFP $ \node -> do
    BS.useAsCStringLen target $ \(t', tLen) ->
        ac_search_init node t' (fromIntegral tLen)
    buildMatchList node []
    where
        buildMatchList node matches =
            alloca $ \matchLenPtr -> alloca $ \matchIDPtr -> do
                ccharPtr <- ac_search node matchLenPtr matchIDPtr
                mcchar <- maybePeek peek ccharPtr
                case mcchar of
                    Just cchar -> do
                        len <- peek matchLenPtr
                        matchStr <- BS.packCStringLen
                            (ccharPtr, fromIntegral len)
                        buildMatchList node (matchStr:matches)
                    Nothing -> return $ reverse matches