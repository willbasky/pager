{-# LANGUAGE MultiWayIf, LambdaCase, OverloadedStrings #-}

-- | 
-- Module      : System.Pager
-- Description : Send stuff to the user's $PAGER.
-- Copyright   : Copyright (c) 2015, Peter Harpending.
-- License     : BSD2
-- Maintainer  : Peter Harpending <peter@harpending.org>
-- Stability   : experimental
-- Portability : Tested with GHC on Linux and FreeBSD
-- 
-- This module is intended to be @import qualified@ed.
-- 

module System.Pager where

import Control.Monad (forM)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Text (Text)
import qualified Data.Monoid (mconcat)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Safe
import System.Directory
import System.IO.Temp
import System.Posix.ByteString

-- |This finds the user's @$PAGER@. This will fail if:
-- 
-- * There is no @$PATH@ variable
-- * The user doesn't have a @less@ or @more@ installed, and hasn't
--   specified an alternate program via @$PAGER@.
-- 
findPager :: IO ByteString
findPager =
  getEnv "PAGER" >>=
  \case
    Just x -> return x
    Nothing ->
      getEnv "PATH" >>=
      \case
        Nothing ->
          fail "There is no $PATH, so I can't see if 'less' or 'more' is installed."
        Just p ->
          do let pathText = TE.decodeUtf8 p
                 pathPieces =
                   T.splitOn ":" pathText
             searchForLess <-
               fmap mconcat
                    (forM pathPieces
                          (\pathPiece ->
                             do dirExists <- doesDirectoryExist (T.unpack pathPiece)
                                filesInDir <- if | dirExists -> getDirectoryContents (T.unpack pathPiece)
                                                 | otherwise -> return mempty
                                return (filter (\x ->
                                                  (x == "less") ||
                                                  (x == "more"))
                                               filesInDir)))
             if |  searchForLess == mempty ->
                  fail "There doesn't appear to be any pager installed."
                |  elem "less" searchForLess ->
                  return "less"
                |  otherwise -> return "more"
