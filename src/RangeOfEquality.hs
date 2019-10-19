{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

{- |
Create a datatype where equality is based on a range of values. This range can be set at runtime, ultimately via a Reader or RIO monad.

This will demonstrate the way that I can work with Vertex within the HasMesh project. 
It will act the same way that I used ChampCad: Point == Point where I used to the closest .01.
-}

module RangeOfEquality
  ( -- * Test the equal function.
    --
    -- | Based on the internal value in the Vertex datatype, using the concept of a range of equality, such as being within 1/100th of each other.
    -- | The ability to adjust the range does not yet exsit. It is hardcoded into the "equal" function.
    Vertex (..),equal, test2, test3, test6, test4, test5,
    -- * Truncation
    -- $Truncation
    Truncation(..), test2a, test2b, test2c,
    -- * Reader Env
    -- $Reader
    App(..)
    ) where

import RIO
import Test.HUnit
import qualified Prelude as P




data Vertex
  -- | Represent a the datatype to compare for equality.
  = Vertex {value :: Double -- ^ The value which is to be compared.
           }
  deriving (Eq, Show)


-- | Take 2 Vertex and compare equality based on the inner value, where they are equal to each other, based on a range, which is hardcoded in, as within 1/100th.
equal :: Vertex -> Vertex -> Bool
equal  (Vertex a) (Vertex b) =
  (abs (a - b)) <= 0.01 


-- | Vertex 1.0 is == Vertex 1.0
test2 = TestCase $ assertEqual
  "test2"
  (True)
  ((Vertex 1.0) `equal` (Vertex 1.0))
--runTest2 = runTestTT test2

-- | (Vertex 1.0) == (Vertex 1.00999999) as they are (barely) within 1/100 
test3 = TestCase $ assertEqual
  "test3"
  (True)
  ((Vertex 1.0) `equal` (Vertex 1.00999999999))

-- | Vertex 1.009 == Vertex 1.0 as they are within 1/100
test6 = TestCase $ assertEqual
  "test6"
  (True)
  ((Vertex 1.009) `equal` (Vertex 1.0))

-- | Vertex 1.0 !== Vertex 1.01 as they are >= 1/100 apart.
test4 = TestCase $ assertEqual
  "test4"
  (False)
  ((Vertex 1.0) `equal` (Vertex 1.01))


-- | Vertex 1.01 !== Vertex 1.0 as they are >= 1/100 apart.
test5 = TestCase $ assertEqual
  "test5"
  (False)
  ((Vertex 1.01) `equal` (Vertex 1.0))

-- $Truncation
--
-- Create a new "equal" fx that allows the range of equality to set.

-- | Setting the range of equality between Vertex using the equalWithTrucation fx.
data Truncation
  -- | Less than 1/10th
  = Tenths
  -- | Less than 1/100th
  | Hundredths


equalWithTrucation :: Truncation -> Vertex -> Vertex -> Bool
equalWithTrucation Tenths   (Vertex a) (Vertex b) =
  (abs (a - b)) <= 0.1

equalWithTrucation Hundredths (Vertex a) (Vertex b) =
  (abs (a - b)) <= 0.01

-- | Vertex 1.0 (equalWithTrucation Tenths) Vertex 1.0
--
-- They are == as they are exactly the same.
test2a = TestCase $ assertEqual
  "test2a"
  (True)
  ((equalWithTrucation Tenths)(Vertex 1.0)  (Vertex 1.0))

-- | Vertex 1.09 (equalWithTrucation Tenths) Vertex 1.0
--
-- They are == as they are 9 100th apart, which is still 1 100th away from being 1 10th.
test2b = TestCase $ assertEqual
  "test2b"
  (True)
  ((equalWithTrucation Tenths)(Vertex 1.09)  (Vertex 1.0))


-- | Vertex 1.1 (equalWithTrucation Tenths) Vertex 1.0
--
-- They are !== as they are 1 10th apart.
test2c = TestCase $ assertEqual
  "test2b"
  (False)
  ((equalWithTrucation Tenths)(Vertex 1.1)  (Vertex 1.0))



-- $Reader
-- Now use Reader App in order to set the Truncation value using the Reader environment

-- | The Reader environment used to supply a equalWithTrucation fx that has the Truncation value curried in.
data App = App
             {_vertexEqual :: Vertex -> Vertex -> Bool -- ^ The definition of equalWithTrucation but with Truncation value curried in.
             }

             
class HasTruncation env where
  --truncatorL :: Lens' env Truncation
  vertexEqual :: Lens' env (Vertex -> Vertex -> Bool)


instance HasTruncation App where
  vertexEqual = lens _vertexEqual (\x y -> x {_vertexEqual = y})

-- | Test for Truncated equality from inside a Reader monad.
-- Vertex 1.0 is (equalWithTrucation Tenths) Vertex 1.0
test7a = TestCase $ assertEqual
  "test7a"
  (True)
  (let 
           env = App $ equalWithTrucation Tenths
           reader ::  Reader App Bool
           reader = do
             t <- view vertexEqual
             return $ t (Vertex 1.0)  (Vertex 1.0)
             
   in   
   runReader reader env 
  )

-- | Test for Truncated equality from inside a Reader monad.
-- | Vertex 1.09 is (equalWithTrucation Tenths) Vertex 1.0
-- Is < 1/10 different so ==
test7b = TestCase $ assertEqual
  "test7b"
  (True)
  (let 
           env = App $ equalWithTrucation Tenths
           reader ::  Reader App Bool
           reader = do
             t <- view vertexEqual
             return $ t (Vertex 1.09)  (Vertex 1.0)
             
   in   
   runReader reader env 
  )

-- | Test for Truncated equality from inside a Reader monad.
-- | Vertex 1.1 is (equalWithTrucation Tenths) Vertex 1.0
-- Is 1/10 different so !==
test7c = TestCase $ assertEqual
  "test7c"
  (False)
  (let 
           env = App $ equalWithTrucation Tenths
           reader ::  Reader App Bool
           reader = do
             t <- view vertexEqual
             return $ t (Vertex 1.1)  (Vertex 1.0)
             
   in   
   runReader reader env 
  )

runTests = do
  --runTest1
  runTestTT test2
  runTestTT test3
  runTestTT test4
  runTestTT test5
  runTestTT test6

  runTestTT test2a
  runTestTT test2b
  runTestTT test2c

  runTestTT test7a
  runTestTT test7b
  runTestTT test7c
  


