module Makefiles where

makeFileRules :: String -> [String]
makeFileRules  = map justRule . filter isRule . lines where
  isRule (' ':_) = False
  isRule ('\t':_) = False
  isRule ('\n':_) = False
  isRule ('#':_) = False
  isRule s = ':' `elem` s && not ('=' `elem` s)
  justRule = takeWhile (/=':')
