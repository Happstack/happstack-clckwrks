{-# LANGUAGE FlexibleContexts, OverloadedStrings, PackageImports, RankNTypes #-}
module Main where

import Clckwrks
-- import Clckwrks.Admin.Template
import Clckwrks.GetOpts          (parseArgs, clckwrksOpts)
import Clckwrks.Server           (simpleClckwrks)
import Clckwrks.Plugin           (clckPlugin)
import Clckwrks.IrcBot.Plugin    (ircBotPlugin)
import Clckwrks.Media.Plugin     (mediaPlugin)
import Control.Applicative       ((<$>))
import Control.Monad             (msum)
import Control.Monad.Trans
import qualified Data.Map         as Map
import Data.Text                  (Text, unpack)
import qualified Data.Text        as Text
import "clckwrks-theme-happstack" Theme
import Web.Routes                 (showURL)
import Web.Plugins.Core           (addHandler, initPlugin, setTheme)
import System.Environment         (getArgs)


------------------------------------------------------------------------------
-- ClckwrksConfig
------------------------------------------------------------------------------

clckwrksConfig :: ClckwrksConfig
clckwrksConfig = ClckwrksConfig
    { clckHostname        = "localhost"
    , clckPort            = 8000
    , clckHidePort        = False
    , clckJQueryPath      = ""
    , clckJQueryUIPath    = ""
    , clckJSTreePath      = ""
    , clckJSON2Path       = ""
    , clckTopDir          = Nothing
    , clckEnableAnalytics = False
    , clckInitHook        = initHook
    }

------------------------------------------------------------------------------
-- main
------------------------------------------------------------------------------

main :: IO ()
main =
    do args <- getArgs
       f    <- parseArgs (clckwrksOpts clckwrksConfig) args
       simpleClckwrks  (f clckwrksConfig)

initHook :: Text
         -> ClckState
         -> ClckwrksConfig
         -> IO (ClckState, ClckwrksConfig)
initHook baseURI clckState cc =
    do let p = plugins clckState
       addHandler p "docs" docHandler
       addHandler p "blog" blogHandler
       initPlugin p baseURI clckPlugin
       initPlugin p baseURI ircBotPlugin
       initPlugin p baseURI mediaPlugin
       setTheme p (Just theme)
       return (clckState, cc)


blogHandler :: ClckPlugins -> [Text] -> ClckT ClckURL (ServerPartT IO) Response
blogHandler _plugins [] =
    do blogURL <- showURL Blog
       seeOther blogURL (toResponse ())
blogHandler _plugins ["atom.xml"] =
    do atomURL <- showURL AtomFeed
       seeOther atomURL (toResponse ())
blogHandler _plugins _ =
    do notFound (toResponse ())


docHandler :: ClckPlugins -> [Text] -> ClckT ClckURL (ServerPartT IO) Response
docHandler _plugins paths =
    localRq (\rq -> rq { rqPaths = map unpack paths }) $
      msum [ dir "crashcourse" $ serveDirectory EnableBrowsing [] "/home/jeremy/public_html/happstack-crashcourse"
           , serveDirectory EnableBrowsing [] "/home/jeremy/public_html/happstack/7"
           ]