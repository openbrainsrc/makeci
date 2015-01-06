module Main where

import System.Environment

main = getArgs >>= dispatch

dispatch ("build":_) = build
dispatch ("test":_) = build
dispatch ("update":_) = build
dispatch ("destroy":_) = build
dispatch ("shell":_) = build
dispatch ("exec":_) = build

build = return ()
