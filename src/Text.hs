{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{- |
Try out the RIO.Text module.
Will also use Data.Text.IO as that did not get re-exported in RIO.
This replaces everthing that would have been done with String, in a much more efficient manner.
Also gives a lot of extra functions that string did not have.
Note the use of {-# LANGUAGE OverloadedStrings #-}, to ensure all Strings are automatically converted to Text, in the code.

However:
Should I work in RIO.ByteString, as it seems to have all the functionality of Text, and avoids the problems of system dependency
mentioned here: https://www.snoyman.com/blog/2016/12/beware-of-readfile
-}

module Text(test1, test1ToStdout, test1WrittenToFile, test2, writeUtf8ToFile, writeUtf8ToFileFromRIO, writeTextToFileFromRIO) where

import RIO
import qualified RIO.Text as RIO.Text
import qualified Data.Text.IO as Data.Text.IO
import qualified GHC.IO.Handle as GHC.IO.Handle
import qualified System.IO as System.IO
import qualified Data.Text.Encoding as Data.Text.Encoding
import qualified RIO.File as RIO.File

import Test.HUnit

filePathToTestFile = "src/Data/TextModuleTest.txt"

runTests = do
  runTestTT test1
  runTestTT test2

-- | Create a Text without any alterations.
test1 = TestCase $ assertEqual
  "test1"
  ("some text")
  ("some text")

-- | Ouput a Text to stdout.
--  Does so without any surrounding quotes.
test1ToStdout =
  Data.Text.IO.putStrLn "some text"

-- | Writes the Text to a file. No surrounding quotes.
--  Overwrites any pre-exising file.
--  Creates a new file if not already exists.
--  Throws an IOException if the Directory does not exist.
test1WrittenToFile = do
  --h <- GHC.IO.Handle.mkFileHandle "src/Data/TextModuleTest.txt" System.IO.WriteMode Nothing GHC.IO.Handle.noNewlineTranslation
  h <- System.IO.openFile filePathToTestFile System.IO.WriteMode
  Data.Text.IO.hPutStr h "some textttt"

-- | Append 2 Text as a monoid.
test2 = TestCase $ assertEqual
  "test2"
  ("some text")
  ("some " <> "text")


-- | Use writeFileUtf8 to write text. Note that it isn't wrapped in double quotes.
--  It automatically encodes the Text to Utf8. If had used RIO.File.writeBinary file,
--  would need to use encodeUtf8, to manually encode the Text to Utf8.
writeUtf8ToFile :: IO ()
writeUtf8ToFile = do
  writeFileUtf8 filePathToTestFile "writeUtf8ToFile2"


--next
--use RIO.File.writeBinaryFile to write the file from the RIO monad.
--This will automatically lift it into RIO monad.
--Do this here and in the RIO test module.

-- | Write Text to a file, from inside the RIO monad using RIO.File.writeBinaryFile.
-- This automatically lifts writeBinaryFile from IO to RIO, however it uses ByteString, so must be converted from Text.
-- What is the overhead of converting. Is it better to stay with Utf8 to avoid Text problems mentiond in :
-- https://www.snoyman.com/blog/2016/12/beware-of-readfile
writeUtf8ToFileFromRIO :: IO ()
writeUtf8ToFileFromRIO =
  runSimpleApp $ RIO.File.writeBinaryFile filePathToTestFile $ Data.Text.Encoding.encodeUtf8 "writeUtf8ToFileFromRIO"

-- | Write Text to a file, from inside the RIO monad using Data.Text.IO.writeFile.
-- The write has to be lifted from IO to RIO, and can use Text directly.
-- But is it better to stick with Utf8 instead of Text as per: https://www.snoyman.com/blog/2016/12/beware-of-readfile
writeTextToFileFromRIO :: IO ()
writeTextToFileFromRIO =
  runSimpleApp $ liftIO $ Data.Text.IO.writeFile filePathToTestFile "writeTextToFileFromRIO"

