{-
run as a script using:
stack runghc -- src/rioTutorial/rioTutorial.hs
or cd into src/rioTutorial and use: stack runghc -- rioTutorial.hs
see https://tech.fpcomplete.com/haskell/tutorial/stack-script for how to script.

from tutorial:
https://tech.fpcomplete.com/haskell/library/rio

================================================================= left off: =============================================
search: Exercise Play around with setting other log settings.
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module RioTutorial() where
--import Prelude (putStrLn)
import qualified Prelude as P

import qualified System.IO as SIO 
import RIO
import RIO.Time (getCurrentTime)

------------------------------------------------------------------------------------------------------------------
sayLastName :: RIO App ()
sayLastName = do
  switchHandle stdout sayHello
  app <- ask
  runRIO app $ addLastName "Weiss" sayBye


sayLastNameWithLog :: RIO App a -> IO a
sayLastNameWithLog inner = do
  logOptions' <- logOptionsHandle stdout False
  let logOptions = setLogUseTime True $ setLogUseLoc True logOptions'
  withLogFunc logOptions $ \logFunc -> do
    let app = App "Justin" stderr logFunc
    runRIO app inner

runSayLastNameWithLog :: IO ()
runSayLastNameWithLog =
  sayLastNameWithLog sayLastName
  
  
  
runApp :: RIO App a -> IO a
runApp inner = do
  logOptions' <- logOptionsHandle stdout False
  let logOptions = setLogUseTime True $ setLogUseLoc True logOptions'
  withLogFunc logOptions $ \logFunc -> do
    let app = App "Heath" stderr logFunc
    runRIO app inner

sayHelloWithLog :: IO ()
sayHelloWithLog = runApp sayEveryThing -- sayHello

sayEveryThing :: RIO App ()
sayEveryThing = do
  sayHello
  sayBye

data App = App
  {appName :: !String,
   appHandle :: !Handle,
   appLogFunc :: !LogFunc
  }

class HasHandle env where
  handleL :: Lens' env Handle

instance HasHandle Handle where
  handleL = id

instance HasHandle App where
  handleL = lens appHandle (\x y -> x {appHandle = y})

class HasName env where
  nameL :: Lens' env String

instance HasName App where
  nameL = lens appName (\x y -> x {appName = y})

say :: HasHandle env =>  String -> RIO env ()
say msg = do
  h <- view handleL
  liftIO $ SIO.hPutStrLn h msg
  

sayHello :: RIO App ()
sayHello = do
  --App name _ <- ask
  name <- view nameL
  say $ "Hello " ++ name

sayBye :: RIO App ()
sayBye = do
  --App name _ <- ask
  name <- view nameL
  say $ "Bye " ++ name

sayTime :: HasHandle env => RIO env ()
sayTime = do
  now <- getCurrentTime
  say $ "The time is " ++ show now

switchHandle :: HasHandle env => Handle -> RIO env a -> RIO env a
switchHandle h = do
  local (set handleL h)
  
addLastName :: HasName env => String -> RIO env a -> RIO env a
addLastName lastName = local $ over nameL (++ " " ++ lastName)

runLogInfo :: IO ()
runLogInfo = runSimpleApp $ do
  logDebug "Debug"
  logInfo "Info"
  logWarn "Warn"
  logError "Error"
