{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE BangPatterns #-} -- for the fibonacci tests
{- |
Explore the RIO.Vector module.


-}

module Vector where

import RIO
import qualified RIO.Vector as V
import RIO.Vector ((!?))
import qualified RIO.Vector.Unboxed as VU
import Test.HUnit

-- * Get a Vector up and running
--
-- $upAndRunning
--
-- Check out some basic functionality such as creating, cons, snoc, and indexs.


-- | Create a Vector from a string and get the length.
test1 = TestCase $ assertEqual
  "test1"
  (2)
  (let
      v :: Vector Text
      v = V.fromList ["one", "two"]
      
   in
   V.length v
  )

-- | Create a Vector from a string and get a value with "find".
test2 = TestCase $ assertEqual
  "test2"
  (Just "one")
  (let
      v :: Vector Text
      v = V.fromList ["one", "two"]
      
   in
   V.find (== "one") v
  )

-- | Create a Vector from a string and get index of a value. Note that the index starts at 0.
test3 = TestCase $ assertEqual
  "test3"
  (Just 0)
  (let
      v :: Vector Text
      v = V.fromList ["one", "two"]
      
   in
   V.findIndex (== "one") v
  )
-- | Prepending a value using cons, changes the index of all pre-existing values.
test4 = TestCase $ assertEqual
  "test4"
  (Just 1)
  (let
      v :: Vector Text
      v = V.fromList ["one", "two"]
      
   in
   V.findIndex (== "one") $ V.cons "three" v
  )

-- | Appending another value to a Vector using snoc, doesn't affect the indexes of the pre-existing values..
test5 = TestCase $ assertEqual
  "test5"
  (Just 0)
  (let
      v :: Vector Text
      v = V.fromList ["one", "two"]
      
   in
   V.findIndex (== "one") $ V.snoc v "three" 
  )

-- | Run the HUnit tests.
--
-- >>> runTests
runTests = do
  runTestTT test1
  runTestTT test2
  runTestTT test3
  runTestTT test4
  runTestTT test5

-- * Performance of boxed(lazy) vs unboxed(strict) Vectors.
--
-- $performance
--
-- Compare the performance of Boxed(lazy) vs UnBoxed(strict) Vectors which are 100 Fibonnaci calculations created with replicate fx.
--
-- Run using :set +s in ghci, which shows the seconds and memory for all fx's run in the current ghci session.
-- This will stay setup untill the ghci session is exited.
--
-- >>> :set +s 



-- ** Setup common values and functions required for the comparison
-- $setup
-- These are common values and functions used by both (boxed/unboxed) Vecotrs required for the comparison.

-- | Length of Vectors being tested. Is only 100, as each value is a costly fibonnacci calculation.
lengthOfTestVector = 100

-- | Supply a thunk, in the form of a fibonnacci calculation, that will take ~0.1 seconds to evaluate.
-- It runs in constant memory of 44,466,712 bytes.
fib :: Int -> Int
fib n = go n (0,1)
  where
    go !n (!a, !b) | n==0      = a
                   | otherwise = go (n-1) (b, a+b)




-- ** Functions for the boxed Vector
-- $boxed
-- Supply values for the boxed(lazy) Vector for working within GHCI.
--
-- The boxed tests require that the test Vector is loaded into ghci with createBoxedVector, and initialized with evaluateBoxedVector.
-- Now the Vector is up and running in ghci. This results in only the access times being evaluated, instead of ghci instantiation time.


-- | Intstantiate a boxed vector in gchi, so the following performance tests will not include creation time of the Vector.
-- Have to make sure it is evaluated first, by running initializeBoxedVector.
-- The use of fib fx results in a Vecotr of thunks.
--
-- >>> let v = createBoxedVector
-- (0.01 secs, 458,304 bytes)
--
createBoxedVector :: Vector Int
createBoxedVector = V.replicate  lengthOfTestVector $ fib 100000 -- (1 + 1)

-- | Force the boxed Vector to be evaluated by ghci, without evaluating the thunks inside the Vector. 
--
-- >>>  evaluateBoxedVector v
-- (0.02 secs, 462,192 bytes)
-- Much faster than evaluating the unboxed Vector, as it does not have to evaluate the thunks.
evaluateBoxedVector :: Vector Int -> Int
evaluateBoxedVector v = V.length v

-- | Evaluate only the 1st thunk in the boxed Vector.
--
-- >>> boxedVectorEvaluateFirstThunk v
-- (0.10 secs, 44,459,952 bytes) the first time it is run, because it has to evaluate the 1st thunk.
--
-- Now run it again now that the 1st thunk has been evaluated.
--
-- >>> boxedVectorEvaluateFirstThunk
-- (0.01 secs, 459,264 bytes)
-- Is 1/10 the time to evaluate this 2nd time, as the first thunk has already been evaluated.
boxedVectorEvaluateFirstThunk :: Vector Int -> Maybe Int
boxedVectorEvaluateFirstThunk v =
  (!?) v 1

-- | Now evaluate all the thunks, by using an equality comparison using the 'all' function.
--
-- In order to get the following #'s, need to exit/restart ghci, otherwise it gives different results.
-- Even just reloading this Vector module, won't do it. Strange
--
-- >>> boxedVectorEvaluateAllThunks v
-- (0.10 secs, 44,466,712 bytes) the first time it is run.
--
-- Now run it a second time.
--
-- >>> boxedVectorEvaluateAllThunks v
-- (0.01 secs, 462,432 bytes) Takes only 1/10th the time to run now that all the thunks have previously been evaluated. 
boxedVectorEvaluateAllThunks :: Vector Int -> Bool
boxedVectorEvaluateAllThunks v =
  V.all (== 2754320626097736315) v

-- ** Functions for the unboxed Vector
-- $unboxed
-- Supply values for the unboxed(strict) Vector for working within GHCI.
-- Be sure to reload ghci, because of the strange results mentioned in 'boxedVectorEvaluateAllThunks'
--
-- Now create and intialize an unboxed vector using the 'createUnBoxedVector' and 'initializeUnBoxedVector' functions.

-- | Create an unboxed vector to ghci for the unboxed tests.
--
-- >>> let v2 = createUnBoxedVector
-- (0.00 secs, 455,272 bytes)
--
-- Has to be initially evaluated with initializeUnBoxedVector
createUnBoxedVector :: VU.Vector Int
createUnBoxedVector = VU.replicate  lengthOfTestVector $ fib 100000 -- (1 + 1)

-- | Evaluate the unboxed Vector to make sure it is ready to go for subsequent tests.
--
-- >>> initializeUnBoxedVector v2
--(0.10 secs, 44,460,904 bytes)
--Much longer to intialize thatn the boxed Vector, as it is evaluating each thunk.
initializeUnBoxedVector :: VU.Vector Int -> Int
initializeUnBoxedVector v = VU.length v

-- | Now look at all the unboxed vector values using  'VU.all'
--
-- >>> unboxedVectorEvaluateAllThunks v2
-- (0.01 secs, 464,352 bytes)
--
-- Now run it a 2nd time
--
-- >>> unboxedVectorEvaluateAllThunks v2
-- (0.01 secs, 464,352 bytes)
-- Runs in the same amount of time, as both times, they were already evaluated.
unboxedVectorEvaluateAllThunks :: VU.Vector Int -> Bool
unboxedVectorEvaluateAllThunks v =
  VU.all (== 2754320626097736315) v



