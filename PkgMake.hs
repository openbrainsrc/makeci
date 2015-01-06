module Main where

import System.Environment

import Makefiles
import Build

main = getArgs >>= dispatch

dispatch ("build":args) = build $ "--bind" `elem` args
dispatch ("test":_) = nop
dispatch ("update":_) = nop
dispatch ("destroy":_) = destroyBuildEnv
dispatch ("shell":args) = shell $ "--bind" `elem` args
dispatch ("exec":_) = nop

nop = return ()

build bind = do
  ensureBuildEnv
  return ()
