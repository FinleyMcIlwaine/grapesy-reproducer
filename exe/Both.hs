module Main where

import Control.Concurrent

import Client
import Server

main :: IO ()
main = do
    _ <- forkIO reproServer
    threadDelay 1_000_000
    reproClient
