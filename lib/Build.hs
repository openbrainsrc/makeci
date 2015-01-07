module Build where

import Makefiles
import Data.Hashable
import System.Directory
import Control.Monad
import System.Process
import Data.List
import System.FilePath

ensureBuildEnv :: Bool -> IO ()
ensureBuildEnv bind = do
  (_, envDir) <- buildEnvDir
  putStrLn envDir
  ex <- doesDirectoryExist envDir
  when (not ex) $ do
    createDirectoryIfMissing True ".pkgmake/env"
    system $ "cowbuilder --create --basepath="++envDir
    putStrLn "Setting up build environment"
    updateEnv bind

updateEnv :: Bool -> IO ()
updateEnv bind = do
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
                                    , "make setup-build-env"
                                    , (if "clean" `elem` rules then "make clean" else "")
                                    ]
  system $ "cowbuilder --execute "++setupFile ++" --save-after-exec --basepath="++envDir++bindMounts
  return ()


buildIt :: Bool -> IO ()
buildIt bind = do
  (ehash, envDir) <- buildEnvDir
  let buildFileName = ".pkgmake/build-"++ehash
  (folder,_) <- createShardFolder bind ehash
  bindMounts <- createBindMounts bind folder
  buildFile folder >>= writeFile buildFileName
  system $ "cowbuilder --execute "++buildFileName ++" --basepath="++envDir++bindMounts
  return ()

shell :: Bool -> IO ()
shell bind = do
  (ehash, envDir) <- buildEnvDir
  (folder,_) <- createShardFolder bind ehash
  bindMounts <- createBindMounts bind folder
  let cmd ="cowbuilder --login --basepath="++envDir++ bindMounts
  putStrLn cmd
  system $ cmd
  return ()

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
     then void $ system $ "ln -sfn "++pwd++" "++(tmpdir</>projName)
     else do
       ex <- doesDirectoryExist (tmpdir</>projName)
       when (not ex) $ void $ system $ "git clone "++pwd++" "++(tmpdir</>projName)
  return (tmpdir, projName)


buildFile :: FilePath -> IO String
buildFile folder = do
  pwd <- getCurrentDirectory
  version <- makeVariableValue "version"

  let projName = last $ splitPath pwd

      checkInstall = "checkinstall -y --pkgversion "++version++" --pkgname="++projName
  return $ unlines ["set -e",
                    "LANG=en_US.UTF-8",
                    "cd "++(folder</>projName),
                    "make",
                    checkInstall
                   ]


destroyBuildEnv :: IO ()
destroyBuildEnv = do
  (ehash, envDir) <- buildEnvDir
  system $ "rm -rf "++envDir
  system $ "rm -rf /tmp/pkgmake/shared-" ++ ehash
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
