{-# LANGUAGE TemplateHaskell #-}
module Main where
import Development.GitRev

gitInfo :: IO ()
gitInfo = putStrLn (concat [ "[", $(gitBranch), "@", $(gitHash)
                           , dirty
                           , " (", $(gitCommitDate), ")", "] "])
  where
    dirty | $(gitDirty) = " __unclean__"
          | otherwise   = ""

main :: IO ()
main = gitInfo
