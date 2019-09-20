{-
from tutorial: https://tech.fpcomplete.com/haskell/tutorial/exceptions

Running Either monad stack in RIO, without using exceptT, by instead throwing exceptions.

Use UnliftIO.Exception instead of Control.Exception module

stack runghc -- rioExceptions.hs

======================================== Left off ================================================
Looking at the tutorial: https://tech.fpcomplete.com/haskell/library/unliftio which is the basis for the RIO module.
It in turn refers me to https://tech.fpcomplete.com/blog/2016/11/covariance-contravariance which is a module in haskell-hacks/misc/covariance.hs
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
import Prelude (putStrLn)
import System.IO (hPutStrLn, stderr)
import qualified System.IO  as SIO
import RIO
import RIO.Time (getCurrentTime)

main :: IO ()
main =
  (runSimpleApp runner)   `catchAny` (\(SomeException e) -> putStrLn " :runSimpleApp runner" )
  
runner :: RIO SimpleApp ()
runner =
  do
    h <- liftIO $ SIO.openFile "" SIO.WriteMode `catchAny` (\e -> do
                                                                   liftIO $ putStrLn "openfile failed"
                                                                   throwIO e
                                                           )
    i <- runEitherRIO (Right 117){-(Left $ MyException "my int error in runner")-}  `catchAny` handleError::RIO SimpleApp (Int) 
    logInfo $ displayShow i
    b <- runEitherRIO (Right True) {-(Left "my bool error in runner")-}-- `catchIO` {-can use `catch`-} handleError::RIO SimpleApp (Bool) -- catchInt -- `catch` \e -> do
    logInfo $ displayShow b
  
data MyException = MyException Text
  deriving (Show, Typeable)

instance Exception MyException


openFile :: RIO SimpleApp (Handle)
openFile = 
  --liftIO $ SIO.openFile "blah\aFile.txt" SIO.WriteMode
  --doesn't throw error despite blah

  --throws an error because of ""
  liftIO $ SIO.openFile "" SIO.WriteMode
  



class ErrorHandler a where
  handleError :: SomeException -> RIO SimpleApp (a)
  
instance ErrorHandler Int where
  handleError (SomeException e) = do
    logInfo $ displayShow e
    throwIO $ MyException ("caught Int:" )
    
  handleError e = throwIO $ MyException "catchBool"

  

class EitherRIO a where
  runEitherRIO :: Either Text a -> RIO SimpleApp (a)

instance EitherRIO Int where
  runEitherRIO (Right int) = do
    case int > 99 of
      True -> throwIO $ MyException "int > 99 not allowed"
      False -> return int
  runEitherRIO (Left e)  = throwIO $ MyException e
                           
instance EitherRIO Bool where
  runEitherRIO (Right bool) = return bool
  runEitherRIO (Left e)  = throwIO $ MyException e
