module Main where

import System.Environment

import Makefiles
import Build

main = getArgs >>= dispatch

dispatch ("build":_) = build
dispatch ("test":_) = nop
dispatch ("update":_) = nop
dispatch ("destroy":_) = nop
dispatch ("shell":_) = nop
dispatch ("exec":_) = nop

nop = return ()

build = do
  ensureBuildEnv
  return ()
