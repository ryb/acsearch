{-# LANGUAGE OverloadedStrings #-}

import Control.Concurrent.ParallelIO.Local
import Data.ByteString.Search.AhoCorasick
import GHC.Conc
import System.FilePath
import Text.Printf
import qualified Data.ByteString.Char8 as BSC

wordList = ["Russia", "Web", "Internet", "marijuana", "Google", "car",
            "exhibit", "song", "album", "dance", "say", "talk"]

passagePaths = map (\n -> "passages" </> (show n)) [1..10]

loadPassages = mapM BSC.readFile passagePaths

runSequentially dictionary passages = mapM (search dictionary) passages

runConcurrently dictionary passages = withPool 10 $ \pool ->
    parallel pool $ map (search dictionary) passages

main = do
    putStrLn $ printf "Running on %d core(s)" numCapabilities
    passages <- loadPassages
    dictionary <- buildDictionary wordList
    sequentialResults <- runSequentially dictionary passages
    concurrentResults <- runConcurrently dictionary passages
    print sequentialResults
    print concurrentResults
    if sequentialResults == concurrentResults
        then putStrLn "Yes, result match"
        else putStrLn "Results don't match!"
