{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

{- |
Try out exceptions using RIO system.
Ref: https://tech.fpcomplete.com/haskell/tutorial/exceptions
-}

module Exceptions(test1, test2, test3, test4, ) where

import RIO

import Test.HUnit

runTests = do
  runTestTT test1
  runTestTT test2
  runTestTT test3
  runTestTT test4
  --runTestTT test5
 
data MyException = MyException
                 | MyStrException Utf8Builder
  deriving (Typeable)

instance Show MyException where
  show MyException = show "MyException"
  show (MyStrException msg) = show $ utf8BuilderToText msg

instance Exception MyException

-- | Catch throwString with SomeException after being bypassed by MyException. Return a msg as IO (String)
test1 = TestCase
  (do
      let
        throwString' :: IO String
        throwString' = throwString  " temp1 " `catch` (\(MyStrException e) -> return "caught by MyStrException")
                                  `catch` (\(SomeException e) -> return  "caught by SomeException" ) 
      thrown <- throwString'
      assertEqual "test1" "caught by SomeException" thrown
  )


-- | Catch throwString with SomeException e and then use displayException e
test2 = TestCase
  (do
      let
        throwString' :: IO String
        throwString' = throwString  "value passed into throwString" `catch` (\(SomeException e) -> return $ displayException e)
        msg = "Control.Exception.Safe.throwString called with:\n\nvalue passed into throwString\nCalled from: ..."
      thrown <- throwString'
      assertEqual "test2" (take 90 msg) (take 90 thrown) --have to use take, or the line #'s won't match when this file is changed.
  )

-- | Throw MyException using throwIO and catch with `catch` SomeException e, using displayException e for error msg.
test3 = TestCase
  (do
      let
        throwString' :: IO String
        throwString' = throwIO  MyException `catch` (\(SomeException e) -> return $ displayException e)
        
      thrown <- throwString'
      assertEqual "test3" "\"MyException\"" thrown
  )

-- | Throw MyException using throwIO and catch with `catch` with my own error handler.
test4 = TestCase
  (do
      let
        throwString' :: IO String
        throwString' = throwIO  MyException `catch` (\(MyException) -> return $ show MyException)
                                            `catch` (\(SomeException e) -> return  "caught by SomeException")
        
      thrown <- throwString'
      assertEqual "test4" "\"MyException\"" thrown
  )
{- Need to do some testing to understand UTF8Builder, then come back and get this working
-- | Throw MyException using throwIO and catch with `catch` with my own error handler.
test5 = TestCase
  (do
      let
        throwString' :: IO String
        throwString' = throwIO  (MyStrException "jkdfl") `catch` (\(MyStrException e) -> return $ e)
                                            `catch` (\(SomeException e) -> return  "caught by SomeException")
        
      thrown <- throwString'
      assertEqual "test5" "\"MyException\"" thrown
  )
--}
