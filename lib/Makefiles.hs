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


makeDryRun :: [String] -> IO (Either String String)
makeDryRun rls =
  psh "." $ "make -n "++intercalate " " rls++" 2>/dev/null"
