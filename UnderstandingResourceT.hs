#!/usr/bin/env stack
-- stack --resolver lts-8.12 script

{-# LANGUAGE BangPatterns #-}
import qualified Data.ByteString as B
import qualified System.IO as IO

import Criterion.Main

dictWords = "/usr/share/dict/words"

main :: IO ()
main = defaultMain
  [ bench "myReadFileLength" $ nfIO (B.length <$> myReadFileLength dictWords)
  , bench "myFileLength" $ nfIO (myFileLength dictWords)
  ]


--------------------------------------------------------------------------------
-- Reads file into memory and concatenates really stupidly
-- 650ms avg.
myReadFileLength :: String -> IO B.ByteString
myReadFileLength fp = IO.withBinaryFile fp IO.ReadMode $ \h ->
  let loop front = do
        -- Inefficient because it reads 4096 blocks or less
        next <- B.hGetSome h 4096

        -- If it is done, we return, otherwise we concatenate the
        -- pieces Inefficient because of how B.append works and the
        -- fact we read the whole file into memory.
        if B.null next
          then return front
          else loop $ B.append front next
  in loop B.empty


--------------------------------------------------------------------------------
-- Doesn't load whole file into memory
-- 1.52ms avg
myFileLength :: FilePath -> IO Int
myFileLength fp = IO.withBinaryFile fp IO.ReadMode $ \h ->
  let loop !total = do
        next <- B.hGetSome h 4096
        if B.null next
          then return total
          else loop $ total + B.length next
  in loop 0
