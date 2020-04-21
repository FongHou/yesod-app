{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Application
  ( appMain,
    makeFoundation,
    makeLogWare,

    -- * for DevelMain
    getApplicationRepl,
    shutdownApp,

    -- * for GHCI
    handler,
    db,
  )
where

import Api (Config (..), app)
import Control.Monad.Logger (liftLoc, runLoggingT)
import Database.Persist.Postgresql (createPostgresqlPool, pgConnStr, pgPoolSize)
import Handler.Comment
-- Import all relevant handler modules here.
-- Don't forget to add new modules to your cabal file!
import Handler.Common
import Handler.Home
import Handler.Profile
import Import hiding (Proxy (..))
import Language.Haskell.TH.Syntax (qLocation)
import Network.HTTP.Client.Conduit (newManager)
import Network.Wai (Middleware)
import Network.Wai.Handler.Warp
  ( Settings,
    defaultSettings,
    defaultShouldDisplayException,
    getPort,
    runSettings,
    setHost,
    setOnException,
    setPort,
  )
import Network.Wai.Middleware.RequestLogger
  ( Destination (Logger),
    IPAddrSource (..),
    OutputFormat (..),
    destination,
    mkRequestLogger,
    outputFormat,
  )
import System.Log.FastLogger (defaultBufSize, newStdoutLoggerSet, toLogStr)
import Yesod.Core.Types (loggerSet)

-- This line actually creates our YesodDispatch instance. It is the second half
-- of the call to mkYesodData which occurs in Foundation.hs. Please see the
-- comments there for more details.
mkYesodDispatch "App" resourcesApp

-- | This function allocates resources (such as a database connection pool),
-- performs initialization and returns a foundation datatype value. This is also
-- the place to put your migrate statements to have automatic database
-- migrations handled by Yesod.
makeFoundation :: AppSettings -> IO App
makeFoundation appSettings = do
  -- Some basic initializations: HTTP connection manager, logger, and static
  -- subsite.
  appHttpManager <- newManager
  appLogger <- newStdoutLoggerSet defaultBufSize >>= makeYesodLogger
  appStatic <- (if appMutableStatic appSettings
                then staticDevel
                else static) (appStaticDir appSettings)
  -- We need a log function to create a connection pool. We need a connection
  -- pool to create our foundation. And we need our foundation to get a
  -- logging function. To get out of this loop, we initially create a
  -- temporary foundation without a real connection pool, get a log function
  -- from there, and then create the real foundation.
  let mkFoundation appConnPool = let appApi = Api.app Config{ .. }
                                 in App{ .. }
  let tempFoundation = mkFoundation $ error "connPool forced in tempFoundation"
      logFunc = messageLoggerSource tempFoundation appLogger
  -- Create the database connection pool
  pool <- flip runLoggingT logFunc
    $ createPostgresqlPool (pgConnStr $ appDatabaseConf appSettings)
                           (pgPoolSize $ appDatabaseConf appSettings)
  -- Perform database migration using our application's logging settings.
  runLoggingT (runSqlPool (runMigration migrateAll) pool) logFunc
  -- Return the foundation
  return $ mkFoundation pool

-- | Convert our foundation to a WAI Application by calling @toWaiAppPlain@ and
-- applying some additional middlewares.
makeApplication :: App -> IO Application
makeApplication foundation = do
  logWare <- makeLogWare foundation
  -- Create the WAI application and apply middlewares
  appPlain <- toWaiAppPlain foundation
  return $ logWare $ defaultMiddlewaresNoLogging appPlain

makeLogWare :: App -> IO Middleware
makeLogWare foundation =
  mkRequestLogger
    def
      { outputFormat =
          if appDetailedRequestLogging $ appSettings foundation
            then Detailed True
            else
              Apache
                ( if appIpFromHeader $ appSettings foundation
                    then FromFallback
                    else FromSocket
                ),
        destination = Logger $ loggerSet $ appLogger foundation
      }

-- | Warp settings for the given foundation value.
warpSettings :: App -> Settings
warpSettings foundation =
  setPort (appPort $ appSettings foundation)
    $ setHost (appHost $ appSettings foundation)
    $ setOnException
      ( \_req e ->
          when (defaultShouldDisplayException e) $
            messageLoggerSource
              foundation
              (appLogger foundation)
              $(qLocation >>= liftLoc)
              "yesod"
              LevelError
              (toLogStr $ "Exception from Warp " ++ show e)
      )
      defaultSettings

getAppSettings :: IO AppSettings
getAppSettings = loadYamlSettings [configSettingsYml] [] useEnv

-- | The @main@ function for an executable running this site.
appMain :: IO ()
appMain = do
  -- Get the settings from all relevant sources
  settings <- loadYamlSettingsArgs [configSettingsYmlValue] useEnv
  -- Generate the foundation from the settings
  foundation <- makeFoundation settings
  -- Generate a WAI Application from the foundation
  wai <- makeApplication foundation
  -- Run the application with Warp
  runSettings (warpSettings foundation) wai

--------------------------------------------------------------
-- Functions for DevelMain.hs (a way to run the app from GHCi)
--------------------------------------------------------------
getApplicationRepl :: IO (Int, App, Application)
getApplicationRepl = do
  settings <- getAppSettings
  foundation <- makeFoundation settings
  wsettings <- getDevSettings $ warpSettings foundation
  app1 <- makeApplication foundation
  return (getPort wsettings, foundation, app1)

shutdownApp :: App -> IO ()
shutdownApp _ = return ()

---------------------------------------------
-- Functions for use in development with GHCi
---------------------------------------------

-- | Run a handler
handler :: Handler a -> IO a
handler h = getAppSettings >>= makeFoundation >>= flip unsafeHandler h

-- | Run DB queries
db :: ReaderT SqlBackend (HandlerFor App) a -> IO a
db = handler . runDB
