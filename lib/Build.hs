module Build where

import Makefiles
import Data.Hashable
import System.Directory
import Control.Monad
import System.Process
import Data.List
import System.FilePath
import Utils

ensureBuildEnv :: Bool -> IO ()
ensureBuildEnv bind = do
  (_, envDir) <- buildEnvDir
  putStrLn envDir
  ex <- doesDirectoryExist envDir
  when (not ex) $ do
    createDirectoryIfMissing True ".pkgmake/env"
    system' $ "cowbuilder --create --basepath="++envDir
    putStrLn "Setting up build environment"
    updateBuildEnv bind

ensureTestEnv :: Bool -> IO ()
ensureTestEnv bind = do
  (_, envDir) <- testEnvDir
  putStrLn envDir
  ex <- doesDirectoryExist envDir
  when (not ex) $ do
    createDirectoryIfMissing True ".pkgmake/env"
    system' $ "cowbuilder --create --basepath="++envDir
    putStrLn "Setting up build environment"
    updateTestEnv bind

updateBuildEnv :: Bool -> IO ()
updateBuildEnv bind = do
  (ehash, envDir) <- buildEnvDir
  let setupFile = ".pkgmake/setup-"++ehash
  deps <- makeRuleContents "build-depends"
  setupEnv <- makeRuleContents "setup-build-env"
  apts <- makeRuleContents "apt-sources-build"

  rules <- fmap makeFileRules $ readFile "Makefile"

  (folder, projName) <- createShardFolder bind ehash
  bindMounts <- createBindMounts bind folder
  writeFile setupFile $ unlines $ [ "DEBIAN_FRONTEND=noninteractive"
                                    , "LANG=en_US.UTF-8"
                                    , "apt-get install -y --no-install-recommends software-properties-common checkinstall make"
                                    , unlines $ map ("add-apt-repository -y "++) apts
                                    , "apt-get update"
                                    , "apt-get install -y "++intercalate " " deps
                                    , "cd "++(folder</>projName)
                                    , (if "clean" `elem` rules then "make clean" else "")
                                    , "make setup-build-env"
                                    ]
  system' $ "cowbuilder --execute "++setupFile ++" --save-after-exec --basepath="++envDir++bindMounts

updateTestEnv :: Bool -> IO ()
updateTestEnv bind = do
  (ehash, envDir) <- testEnvDir
  let setupFile = ".pkgmake/setup-"++ehash
  deps <- makeRuleContents "depends"
  testdeps <- makeRuleContents "test-depends"
  setupEnv <- makeRuleContents "setup-test-env"
  apts <- makeRuleContents "apt-sources-test"

  rules <- fmap makeFileRules $ readFile "Makefile"

  (folder, projName) <- createShardFolder bind ehash
  bindMounts <- createBindMounts bind folder
  writeFile setupFile $ unlines $ [ "DEBIAN_FRONTEND=noninteractive"
                                    , "LANG=en_US.UTF-8"] ++
                                  install_apt_lines apts++
                                  [ "apt-get install -y "++intercalate " " (deps++testdeps)
                                    , "cd "++(folder</>projName)
                                    , if not $ null setupEnv then "make setup-test-env" else ""
                                    ]
  system' $ "cowbuilder --execute "++setupFile ++" --save-after-exec --basepath="++envDir++bindMounts

install_apt_lines [] = []
install_apt_lines apts =
  [ "apt-get install -y --no-install-recommends software-properties-common"
  , unlines $ map ("add-apt-repository -y "++) apts
  , "apt-get update"]


buildIt :: Bool -> IO String
buildIt bind = do
  (ehash, envDir) <- buildEnvDir
  let buildFileName = ".pkgmake/build-"++ehash
  (folder,_) <- createShardFolder bind ehash
  bindMounts <- createBindMounts bind folder
  (bfile, debPath) <- buildFile folder
  writeFile buildFileName bfile
  system' $ "cowbuilder --execute "++buildFileName ++" --basepath="++envDir++bindMounts
  return debPath

testIt :: Bool -> IO ()
testIt bind = do
  ensureTestEnv bind
  (ehash, envDir) <- testEnvDir
  (build_hash, _) <- buildEnvDir
  let buildFileName = ".pkgmake/test-"++ehash
  (folder,_) <- createShardFolder bind build_hash
  bindMounts <- createBindMounts bind folder


  --here, ensure deb is present, otherwise build it

  pwd <- getCurrentDirectory
  version <- makeVariableValue "version"

  let projName = last $ splitPath pwd

  let debPath' = "/tmp/pkgmake/shared-" ++ (build_hash </>projName</>(projName++"_"++version++"-1_amd64.deb"))
  ex <- doesFileExist debPath'
  debPath <- if ex
                then return debPath'
                else do
                  ensureBuildEnv bind
                  buildIt bind

  --also install deb
  writeFile buildFileName $ unlines ["dpkg -i "++debPath, "cd "++(folder</>projName),"make test"]
  system' $ "cowbuilder --execute "++buildFileName ++" --basepath="++envDir++bindMounts

shell :: Bool -> IO ()
shell bind = do
  (ehash, envDir) <- buildEnvDir
  (folder,_) <- createShardFolder bind ehash
  bindMounts <- createBindMounts bind folder
  let cmd = "cowbuilder --login --basepath="++envDir++ bindMounts
  system' $ cmd

createBindMounts :: Bool -> FilePath -> IO String
createBindMounts bind folder = do
  pwd <- getCurrentDirectory -- TODO make this top directory
  if bind
     then return $ " --bindmounts=\""++pwd++" "++folder++"\""
     else return $ " --bindmounts="++folder



createShardFolder :: Bool -> String -> IO (FilePath, String)
createShardFolder bind ehash = do
  let tmpdir = "/tmp/pkgmake/shared-" ++ ehash
  pwd <- getCurrentDirectory
  let projName = last $ splitPath pwd
  createDirectoryIfMissing True tmpdir
  if bind
     then void $ system' $ "ln -sfn "++pwd++" "++(tmpdir</>projName)
     else do
       ex <- doesDirectoryExist (tmpdir</>projName)
       when (not ex) $ void $ system' $ "git clone "++pwd++" "++(tmpdir</>projName)
  return (tmpdir, projName)


buildFile :: FilePath -> IO (String, FilePath)
buildFile folder = do
  (ehash, _) <- buildEnvDir
  pwd <- getCurrentDirectory
  version <- makeVariableValue "version"
  deps <- fmap createDeps $ makeRuleContents "depends"
  let projName = last $ splitPath pwd

      checkInstall = "checkinstall -y --pkgversion "++version++" --install=no --pkgname="++projName++deps
      debPath = "/tmp/pkgmake/shared-" ++ (ehash </>projName</>(projName++"_"++version++"-1_amd64.deb"))
      bfile = unlines ["set -e",
                    "LANG=en_US.UTF-8",
                    "cd "++(folder</>projName),
                    "make",
                    checkInstall
                   ]
  return (bfile,debPath)

createDeps :: [String] -> String
createDeps [] = ""
createDeps ss = " --requires=\""++(intercalate "," $ map chomp ss)++"\""


destroyBuildEnv :: IO ()
destroyBuildEnv = do
  (ehash, envDir) <- buildEnvDir
  system' $ "rm -rf "++envDir
  system' $ "rm -rf /tmp/pkgmake/shared-" ++ ehash

buildEnvDir :: IO (String, FilePath)
buildEnvDir = envDirFromRules $ words "build-depends setup-build-env"

testEnvDir :: IO (String, FilePath)
testEnvDir = envDirFromRules ["depends test-depends setup-test-env"]

envDirFromRules :: [String] -> IO (String, FilePath)
envDirFromRules defrules = do
  allrules <- fmap makeFileRules $ readFile "Makefile"
  --mapM putStrLn rules
  let envRules = filter (`elem` allrules) defrules
  buildEnv <- makeDryRun envRules
  let ehash = show $ abs $ hash buildEnv
  return $ (ehash, ".pkgmake/env/"++ehash)
