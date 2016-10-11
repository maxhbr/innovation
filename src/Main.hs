{-# LANGUAGE TemplateHaskell #-}
module Main where
import Development.GitRev

gitInfo :: IO ()
gitInfo = putStrLn (concat [ "[", $(gitBranch), "@", $(gitHash)
                           , " (", $(gitCommitDate), ")"
                           , " (", $(gitCommitCount), " commits in HEAD)"
                           , dirty, "] "])
  where
    dirty | $(gitDirty) = " (unclean)"
          | otherwise   = ""

main :: IO ()
main = gitInfo
