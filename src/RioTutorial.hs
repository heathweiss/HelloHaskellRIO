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

{-
The ADT passed into RIO app.
Used same as a Reader monad, which is what RIO is: ReaderT IO
Notes:
Everything is strict.
Uses a LogFunc, which is recommended way of logging.
-}
data App = App
  {appName :: !String,
   appHandle :: !Handle,
   appLogFunc :: !LogFunc
  }

--------------------------------------------------LogFunc----------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------------------------------
{-
search: Alright, enough playing around. How do we actually create a log function?!? We go through a two-stage process:


---------------------------------How to run RIO using LogOptions:--------------------------------
Purpose of LogOption:
Setting up logging, including debug logs.
Run a RIO app using LogOption

1:create the runApp fx. A new one has to be created for every variation of LogFunc and App env that will be run.
2 Call a RIO APP () using the runApp fx.
-}

{-
Create/setup the LogFunc system.
Create the App env.
Run whatever RIO App is passed in.
-}  
runApp :: RIO App a -> IO a
runApp inner = do
  logOptions' <- logOptionsHandle stdout False
  let logOptions = setLogUseTime True $ setLogUseLoc True logOptions'
  withLogFunc logOptions $ \logFunc -> do
    let app = App "Heath" stderr logFunc
    runRIO app inner

--Use runApp 
runSayThingsWitLog :: IO ()
runSayThingsWitLog = runApp sayEveryThingWithLog -- sayHello

sayEveryThingWithLog :: RIO App ()
sayEveryThingWithLog = do
  sayHello
  sayBye
  --alternatively add the last name.
  app <- ask
  runRIO app $ addLastName "Weiss" sayBye

  
----------------------------------------------------end: logFunc---------------------------------------------------------------------------


-------------------------------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------------------------------
---------------------------------------------- setting up HasHandle tpeclass - ------------------------------------------------------------

{-
The App field (appHandle :: !Handle) is used as and example of setting up a reusable field.
The (<appHandle/or other name> :: !Handle) could be put into any ADT, and used by implementing this typeclass system.

Steps involved are:
Set up the HasHandle typeclass.
Make the target ADT and instance of the HasHandle typeclass.
-}

--declare the typeclass.
class HasHandle env where
  handleL :: Lens' env Handle

--Make Handle and instance of HasHandle. Not sure why this has to be done.
--In the case of HasName typeclass, no such thing has to be done.
instance HasHandle Handle where
  handleL = id

--Make App and instance of HasHandle.
instance HasHandle App where
  handleL = lens appHandle (\x y -> x {appHandle = y})

--another example useing a String. Note that didn't have to make string an instance the way Handle had to be declared.
class HasName env where
  nameL :: Lens' env String

instance HasName App where
  nameL = lens appName (\x y -> x {appName = y})

---------------------------------------------- end: setting up HasHandle tpeclass - --------------------------------------------------------



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


