module Build where

import Makefiles
import Utils
import Data.Hashable
import System.Directory
import Control.Monad
import System.Process
import Data.List

ensureBuildEnv :: IO ()
ensureBuildEnv = do
  (ehash, envDir) <- buildEnvDir
  putStrLn envDir
  let setupFile = ".pkgmake/setup-"++ehash
  ex <- doesDirectoryExist envDir
  when (not ex) $ do
    --createDirectoryIfMissing True envDir
    system $ "cowbuilder --create --basepath="++envDir
    putStrLn "Setting up build environment"
    deps <- makeRuleContents "build-depends"
    setupEnv <- makeRuleContents "setup-build-env"
    writeFile setupFile $ unlines $ [ "DEBIAN_FRONTEND=noninteractive"
                                    , "apt-get update"
                                    , "apt-get install -y "++intercalate " " deps ]++setupEnv
    system $ "cowbuilder --execute "++setupFile ++" --save-after-exec --basepath="++envDir
    return ()

shell :: Bool -> IO ()
shell nobind = do
  (_, envDir) <- buildEnvDir
  pwd <- getCurrentDirectory
  let addBind = if nobind
                   then ""
                   else " --bindmounts="++pwd
  system $ "cowbuilder --login --basepath="++envDir++addBind
  return ()


destroyBuildEnv :: IO ()
destroyBuildEnv = do
  (_, envDir) <- buildEnvDir
  system $ "rm -rf "++envDir
  return ()

buildEnvDir :: IO (String, FilePath)
buildEnvDir = do
  ehash <- buildEnvHash
  return $ (ehash, ".pkgmake/env/"++ehash)

buildEnvHash :: IO String
buildEnvHash = do
  rules <- fmap makeFileRules $ readFile "Makefile"
  --mapM putStrLn rules
  let envRules = filter (`elem` rules) $ words "build-depends setup-build-env"
  buildEnv <- makeDryRun envRules
  return $ show $ hash buildEnv
