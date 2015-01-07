module Main where

import System.Environment

import Makefiles
import Build

main = getArgs >>= dispatch

dispatch ("build":args) = build $ not $ "--clone" `elem` args
dispatch ("test":args) = testIt $ not $ "--clone" `elem` args
dispatch ("updateenv":args) = updateBuildEnv $ not $ "--clone" `elem` args
dispatch ("destroy":_) = destroyBuildEnv
dispatch ("shell":args) = shell $ not $ "--clone" `elem` args
dispatch ("exec":_) = nop

nop = return ()

build bind = do
  ensureBuildEnv bind
  buildIt bind

  return ()
