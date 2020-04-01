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

module Text {-(test1, test1ToStdout, test1WrittenToFile, test2, writeUtf8ToFile, writeUtf8ToFileFromRIO, writeTextToFileFromRIO)-} where

import RIO
import qualified RIO.Text as T
--import qualified Data.Text.IO as Data.Text.IO
import qualified Prelude as P
import qualified GHC.IO.Handle as GHC.IO.Handle
import qualified System.IO as System.IO
--import qualified Data.Text.Encoding as Data.Text.Encoding
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

-- * Use Text with IO
--
-- $useIO
--
-- Output Text in IO using various IO functions such as putStrLn and hPutStr

-- | Ouput a Text to stdout.
--  Does so without any surrounding quotes.
test1ToStdout =
  P.putStrLn "some text"

-- | Writes the Text to a file. No surrounding quotes.
--  Overwrites any pre-exising file.
--  Creates a new file if not already exists.
--  Throws an IOException if the Directory does not exist.
{-
Need to write these using RIO system to write text to a file.
test1WrittenToFile = do
  --h <- GHC.IO.Handle.mkFileHandle "src/Data/TextModuleTest.txt" System.IO.WriteMode Nothing GHC.IO.Handle.noNewlineTranslation
  h <- System.IO.openFile filePathToTestFile System.IO.WriteMode
  hPutStr h "some text"

-- | Write Text to a file, from inside the RIO monad using T.writeFile.
-- The write has to be lifted from IO to RIO, and can use Text directly.
-- But is it better to stick with Utf8 instead of Text as per: https://www.snoyman.com/blog/2016/12/beware-of-readfile?
-- Utf8 seems to have all the same functions as the Text module.
writeTextToFileFromRIO :: IO ()
writeTextToFileFromRIO =
  runSimpleApp $ liftIO $ writeFile filePathToTestFile "writeTextToFileFromRIO"
-}
-- * Use Text as an instance of Monoid
--
-- $useAsMonoid
--
-- Manipulate Text as a Monoid.


-- | Append Text as a monoid.
test2 = TestCase $ assertEqual
  "test2"
  ("some text")
  ("some " <> "text")



