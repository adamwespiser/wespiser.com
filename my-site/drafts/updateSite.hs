#!/usr/bin/env stack
{- stack script
 --compile
 --copy-bins
 --resolver lts-14.17
 --install-ghc
 --package turtle
 --package text
 --package foldl
 --package async
 --ghc-options=-Wall
-}
{-# LANGUAGE OverloadedStrings #-}

import qualified Turtle as Tu
import qualified Control.Foldl as L
import qualified Data.Text as T
import Control.Concurrent.Async
import System.IO

argParser :: Tu.Parser Tu.FilePath
argParser = Tu.argPath "html" "html destination directory"

main :: IO ()
main = do
  -- 53 files copied over into destinationDir
  hSetBuffering stdout NoBuffering
  destinationDir <- Tu.options "Build blog and copy to directory" argParser
  Tu.with (Tu.mktempdir "/tmp" "deploy") (mainLoop destinationDir)

mainLoop :: Tu.FilePath -> Tu.FilePath -> IO ()
mainLoop destDir tempDir = do

  _ <- concurrently (downloadGitRepo "https://github.com/write-you-a-scheme-v2/scheme.git")
               (downloadGitRepo "https://github.com/adamwespiser/wespiser.com.git")

  copyHelper "scheme/docs/" "wespiser.com/my-site/writings/wyas/"
  copyHelper "scheme/img/" "wespiser.com/my-site/img/"

  Tu.cd (tempDir Tu.</> "wespiser.com/my-site")
  _ <- Tu.shell "stack build --fast --ghc-options=\"-w\"" Tu.empty
  _ <- Tu.shell "stack exec site rebuild" Tu.empty

  copyBuildToTarget

  where
    downloadGitRepo :: (Tu.MonadIO m) => String -> m ()
    downloadGitRepo gitrepo = do
      let gitDirectory = Tu.dropExtension . Tu.filename . Tu.decodeString $ gitrepo
          gitPath = tempDir Tu.</> gitDirectory
          gitCommand = "git clone " ++ gitrepo ++ " --depth 1 " ++  Tu.encodeString (tempDir Tu.</> gitDirectory)
      Tu.testdir gitPath >>= (\x -> Tu.when x $ Tu.rmtree gitPath) 
      Tu.shell (T.pack gitCommand) Tu.empty >> pure ()

    copyHelper :: (Tu.MonadIO m) => Tu.FilePath -> Tu.FilePath -> m ()
    copyHelper src trg = Tu.foldIO (Tu.ls $ tempDir Tu.</> src)
        (L.sink $ \f -> Tu.cp f $ tempDir Tu.</> trg Tu.</> Tu.filename f)

    copyBuildToTarget :: (Tu.MonadIO m) => m ()
    copyBuildToTarget = do
      Tu.foldIO (Tu.ls destDir) (L.sink Tu.rmtree)
      Tu.foldIO (Tu.ls $ tempDir Tu.</> "wespiser.com/my-site/_site")
        (L.sink $ procCopyBuildToTarget destDir)

    procCopyBuildToTarget :: (Tu.MonadIO m) => Tu.FilePath -> Tu.FilePath -> m ()
    procCopyBuildToTarget dest file = do
      src <- fromEither $ Tu.toText file
      trg <- fromEither $ Tu.toText $ dest Tu.</> Tu.filename file
      Tu.procs "cp" [T.pack "-r", src, trg] Tu.mempty
      where
        fromEither = either (error . T.unpack) pure
