module Build where

import Makefiles
import Utils
import Data.Hashable

ensureBuildEnv = do
  hash <- buildEnvHash
  print hash
  return ()

buildEnvHash = do
  rules <- fmap makeFileRules $ readFile "Makefile"
  --mapM putStrLn rules
  let envRules = filter (`elem` rules) $ words "build-depends setup-build-env"
  mbuildEnv <- makeDryRun envRules
  case mbuildEnv of
   Left e -> fail $  "error: "++e
   Right b -> return $ hash b
