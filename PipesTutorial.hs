#!/usr/bin/env stack
-- stack script --resolver lts-8.12 --package pipes  --package text

{-# LANGUAGE OverloadedStrings #-}

import           Prelude  hiding (writeFile)
import           Data.Text hiding (empty, null)
import           Control.Monad (unless)
import           Pipes
import           System.IO (isEOF)

--------------------------------------------------------------------------------
-- Outputs document to a test3.xml file

stdinLn :: Producer String IO ()
stdinLn = do
    eof <- lift isEOF        -- 'lift' an 'IO' action from the base monad
    unless eof $ do
        str <- lift getLine
        yield str            -- 'yield' the 'String'
        stdinLn              -- Loop

main :: IO ()
main = putStrLn "Hello"
