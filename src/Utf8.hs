{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{- |
Use the Uft8 module.
-}
module Utf8 where


import RIO
import qualified RIO.File as RIO.File
--import qualified Data.Text.Encoding as Data.Text.Encoding

filePathToTestFile = "src/Data/TextUtf8Test.txt"

-- * Write Utf8 out to a file
--
-- $writeUtf8ToFile
--
-- Write Utf8 out to a file.

-- | Use writeFileUtf8 to write text. Note that it isn't wrapped in double quotes.
--  It automatically encodes the Text to Utf8. If had used RIO.File.writeBinary file,
--  would need to use encodeUtf8, to manually encode the Text to Utf8.
--
-- writeFileUtf8 comes from the RIO prelude.
writeUtf8ToFile :: IO ()
writeUtf8ToFile = do
  writeFileUtf8 filePathToTestFile "writeUtf8ToFile2"


-- | Write Text to a file, from inside the RIO monad using RIO.File.writeBinaryFile.
-- This automatically lifts writeBinaryFile from IO to RIO, however it uses ByteString, so must be converted from Text.
-- What is the overhead of converting. Is it better to stay with Utf8 to avoid Text problems mentiond in :
-- https://www.snoyman.com/blog/2016/12/beware-of-readfile
{-re-write this with RIO system for creating Utf8, probably with Uft8Builder
writeUtf8ToFileFromRIO :: IO ()
writeUtf8ToFileFromRIO =
  runSimpleApp $ RIO.File.writeBinaryFile filePathToTestFile $ Data.Text.Encoding.encodeUtf8 "writeUtf8ToFileFromRIO"
-}

