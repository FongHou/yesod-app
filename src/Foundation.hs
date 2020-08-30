{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Foundation where

import qualified Data.CaseInsensitive as CI
import qualified Data.Text.Encoding as TE
import Facebook (Credentials (..))
import Import.NoFoundation
import Network.HTTP.Client (HasHttpManager (..), Manager)
import Text.Hamlet (hamletFile)
import Text.Jasmine (minifym)
import Yesod.Auth.Dummy (authDummy)
import Yesod.Auth.Facebook.ServerSide
import qualified Yesod.Auth.Message as Msg
import Yesod.Core.Types (Logger)
import qualified Yesod.Core.Unsafe as Unsafe
import Yesod.Default.Util (addStaticContentExternal)
import Yesod.Facebook

mkYesodData
  "App"
  [parseRoutes| -- $(parseRoutesFile "config/routes")

/static StaticR   Static      appStatic
/auth   AuthR     Auth        getAuth
/api    ServantR  WaiSubsiteWithAuth  getServant

/favicon.ico FaviconR GET
/robots.txt RobotsR GET

/ HomeR GET

/profile ProfileR GET

  |]

-- | The foundation datatype for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data App = App
  { appSettings :: AppSettings,
    appStatic :: Static,
    appApi :: Application,
    appConnPool :: ConnectionPool,
    appSecrets :: [(Text, Text)],
    appHttpManager :: Manager,
    appLogger :: Logger
  }
  deriving (Generic)

data MenuItem = MenuItem
  { menuItemLabel :: Text,
    menuItemRoute :: Route App,
    menuItemAccessCallback :: Bool
  }

data MenuTypes
  = NavbarLeft MenuItem
  | NavbarRight MenuItem

-- | A convenient synonym for database access functions.
type DB a = forall m. MonadIO m => ReaderT SqlBackend m a

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod App where
  -- approot = ApprootStatic "http://localhost:3000"
  approot = ApprootRequest $ \app req -> case appRoot $ appSettings app of
    Nothing -> getApprootText guessApproot app req
    Just root -> root

  -- Store session data on the client in encrypted cookies,
  -- default session idle timeout is 120 minutes
  makeSessionBackend _ =
    Just <$> defaultClientSessionBackend 120 "config/client_session_key.aes"

  -- Yesod Middleware allows you to run code before and after each handler function.
  -- The defaultYesodMiddleware adds the response header "Vary: Accept, Accept-Language" and performs authorization checks.
  -- Some users may also want to add the defaultCsrfMiddleware, which:
  --   a) Sets a cookie with a CSRF token in it.
  --   b) Validates that incoming write requests include that token in either a header or POST parameter.
  -- To add it, chain it together with the defaultMiddleware: yesodMiddleware = defaultYesodMiddleware . defaultCsrfMiddleware
  -- For details, see the CSRF documentation in the Yesod.Core.Handler module of the yesod-core package.
  yesodMiddleware = defaultYesodMiddleware

  defaultLayout widget = do
    master <- getYesod
    muser <- maybeAuthPair
    mmsg <- getMessage
    mcurrentRoute <- getCurrentRoute
    -- Get the breadcrumbs, as defined in the YesodBreadcrumbs instance.
    (_title, _parents) <- breadcrumbs
    -- Define the menu items of the header.
    let menuItems =
          [ NavbarLeft $
              MenuItem
                { menuItemLabel = "Home",
                  menuItemRoute = HomeR,
                  menuItemAccessCallback = True
                },
            NavbarLeft $
              MenuItem
                { menuItemLabel = "Profile",
                  menuItemRoute = ProfileR,
                  menuItemAccessCallback = isJust muser
                },
            NavbarRight $
              MenuItem
                { menuItemLabel = "Login",
                  menuItemRoute = AuthR LoginR,
                  menuItemAccessCallback = isNothing muser
                },
            NavbarRight $
              MenuItem
                { menuItemLabel = "Logout",
                  menuItemRoute = AuthR LogoutR,
                  menuItemAccessCallback = isJust muser
                }
          ]
    let navbarLeftMenuItems = [x | NavbarLeft x <- menuItems]
    let navbarRightMenuItems = [x | NavbarRight x <- menuItems]
    let navbarLeftFilteredMenuItems =
          [x | x <- navbarLeftMenuItems, menuItemAccessCallback x]
    let navbarRightFilteredMenuItems =
          [x | x <- navbarRightMenuItems, menuItemAccessCallback x]
    -- We break up the default layout into two components:
    -- default-layout is the contents of the body tag, and
    -- default-layout-wrapper is the entire page. Since the final
    -- value passed to hamletToRepHtml cannot be a widget, this allows
    -- you to use normal widget features in default-layout.
    pc <- widgetToPageContent $ do
      $(widgetFile "default-layout")
    withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")

  -- The page to be redirected to when authentication is required.
  authRoute _ = Just $ AuthR LoginR

  -- Routes not requiring authentication.
  isAuthorized (AuthR _) _ = return Authorized
  isAuthorized HomeR _ = return Authorized
  isAuthorized FaviconR _ = return Authorized
  isAuthorized RobotsR _ = return Authorized
  isAuthorized (StaticR _) _ = return Authorized
  isAuthorized (ServantR _) _ = return Authorized
  -- Routes requiring authentication.
  isAuthorized ProfileR _ = return Authorized

  -- This function creates static content files in the static folder
  -- and names them based on a hash of their content. This allows
  -- expiration dates to be set far in the future without worry of
  -- users receiving stale content.
  addStaticContent ext mime content = do
    master <- getYesod
    let staticDir = appStaticDir $ appSettings master
    addStaticContentExternal
      minifym
      genFileName
      staticDir
      (StaticR . flip StaticRoute [])
      ext
      mime
      content
    where
      -- Generate a unique filename based on the content itself
      genFileName lbs = "autogen-" ++ base64md5 lbs

  shouldLogIO app _source level =
    return $
      appShouldLogAll (appSettings app)
        || level == LevelWarn
        || level == LevelError

  makeLogger = return . appLogger

-- Define breadcrumbs.
instance YesodBreadcrumbs App where
  breadcrumb HomeR = return ("Home", Nothing)
  breadcrumb (AuthR _) = return ("Login", Just HomeR)
  breadcrumb ProfileR = return ("Profile", Just HomeR)
  breadcrumb _ = return ("home", Nothing)

-- How to run database actions.
instance YesodPersist App where
  type YesodPersistBackend App = SqlBackend

  runDB action = do
    master <- getYesod
    runSqlPool action $ appConnPool master

instance YesodPersistRunner App where
  getDBRunner = defaultGetDBRunner appConnPool

instance YesodFacebook App where
  fbCredentials _ =
    let fbClientId = "fb_client_id"
        fbClientSecret = "fb_client_secret"
     in Credentials "yesod" fbClientId fbClientSecret True

instance YesodAuth App where
  type AuthId App = UserId

  -- Where to send a user after successful login
  loginDest _ = HomeR

  -- Where to send a user after logout
  logoutDest _ = HomeR

  -- Override the above two destinations when a Referer: header is present
  redirectToReferer _ = True

  authenticate creds = liftHandler $ do
    muser <- runDB $ getBy $ UniqueUserUsername (credsIdent creds)
    case muser of
      Nothing -> return $ UserError Msg.InvalidLogin
      Just (Entity uid _) -> return $ Authenticated uid

  authPlugins app = [authFacebook ["user_about_me", "email"]] ++ dummyLogin
    where
      dummyLogin = [authDummy | appAuthDummyLogin $ appSettings app]

  authHttpManager = appHttpManager <$> getYesod

-- | Access function to determine if a user is logged in.
isAuthenticated :: Handler AuthResult
isAuthenticated = do
  muid <- maybeAuthId
  return $ case muid of
    Nothing -> Unauthorized "You must login to access this page"
    Just _ -> Authorized

instance YesodAuthPersist App

instance RenderMessage App FormMessage where
  renderMessage _ _ = defaultFormMessage

instance HasHttpManager App where
  getHttpManager = appHttpManager

getServant :: App -> WaiSubsiteWithAuth
getServant = WaiSubsiteWithAuth . appApi

unsafeHandler :: App -> Handler a -> IO a
unsafeHandler = Unsafe.fakeHandlerGetLogger appLogger
