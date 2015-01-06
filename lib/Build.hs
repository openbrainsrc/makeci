module Build where

import Makefiles
import Utils
import Data.Hashable
import System.Directory
import Control.Monad
import System.Process
import Data.List
import System.FilePath

ensureBuildEnv :: Bool -> IO ()
ensureBuildEnv bind = do
  (ehash, envDir) <- buildEnvDir
  putStrLn envDir
  let setupFile = ".pkgmake/setup-"++ehash
  ex <- doesDirectoryExist envDir
  when (not ex) $ do
    createDirectoryIfMissing True ".pkgmake/env"
    system $ "cowbuilder --create --basepath="++envDir
    putStrLn "Setting up build environment"
    deps <- makeRuleContents "build-depends"
    setupEnv <- makeRuleContents "setup-build-env"
    apts <- makeRuleContents "apt-sources-build"
    (folder, projName) <- createShardFolder bind ehash
    writeFile setupFile $ unlines $ [ "DEBIAN_FRONTEND=noninteractive"
                                    , "apt-get install -y --no-install-recommends software-properties-common checkinstall make"
                                    , unlines $ map ("add-apt-repository -y "++) apts
                                    , "apt-get update"
                                    , "apt-get install -y "++intercalate " " deps
                                    , "cd "++(folder</>projName)
                                    ]++setupEnv
    system $ "cowbuilder --execute "++setupFile ++" --save-after-exec --basepath="++envDir++" --bindmounts="++folder
    return ()

buildIt :: Bool -> IO ()
buildIt bind = do
  (ehash, envDir) <- buildEnvDir
  let buildFileName = ".pkgmake/build-"++ehash
  (folder,_) <- createShardFolder bind ehash
  buildFile folder >>= writeFile buildFileName
  system $ "cowbuilder --execute "++buildFileName ++" --basepath="++envDir++" --bindmounts="++folder
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

createShardFolder :: Bool -> String -> IO (FilePath, String)
createShardFolder bind ehash = do
  let tmpdir = "/tmp/pkgmake/shared-" ++ ehash
  pwd <- getCurrentDirectory
  let projName = last $ splitPath pwd
  createDirectoryIfMissing True tmpdir
  if bind
     then void $ system $ "ln -sfn "++pwd++" "++(tmpdir</>projName)
     else do
       ex <- doesDirectoryExist (tmpdir</>projName)
       when (not ex) $ void $ system $ "git clone "++pwd++" "++(tmpdir</>projName)
  return (tmpdir, projName)


buildFile :: FilePath -> IO String
buildFile folder = do
  (ehash, envDir) <- buildEnvDir
  pwd <- getCurrentDirectory
  let projName = last $ splitPath pwd
  return $ unlines ["cd "++(folder</>projName),
                    "make",
                    --"checkinstall ",
                    "cp"]


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
  return $ show $ abs $ hash buildEnv
