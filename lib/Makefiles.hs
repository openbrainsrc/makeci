module Makefiles where

import Utils
import Data.List

makeFileRules :: String -> [String]
makeFileRules  = map justRule . filter isRule . lines where
  isRule (' ':_) = False
  isRule ('\t':_) = False
  isRule ('\n':_) = False
  isRule ('#':_) = False
  isRule s = ':' `elem` s && not ('=' `elem` s)
  justRule = takeWhile (/=':')


makeDryRun :: [String] -> IO String
makeDryRun rls =
  force_psh $ "make -n "++intercalate " " rls++" 2>/dev/null"

makeVariableValue :: String -> IO String
makeVariableValue varnm = do
  res <- force_psh $ "make -rpn 2>/dev/null | grep "++varnm
  return $ chomp $ tail $ dropWhile (/='=') res

makeRuleContents :: String -> IO [String]
makeRuleContents rule = do
  rules <- fmap makeFileRules $ readFile "Makefile"
  if not $ rule `elem` rules
     then return []
     else fmap (filter (not . mostlyEmpty) . lines) $ makeDryRun [rule]

chomp = reverse . c . reverse . c where
  c = dropWhile crap
  crap ' ' = True
  crap '\r' = True
  crap '\n' = True
  crap '\t' = True
  crap _ = False

getGitBranchName :: IO String
getGitBranchName = do
  fmap chomp $ force_psh "git rev-parse --abbrev-ref HEAD"
