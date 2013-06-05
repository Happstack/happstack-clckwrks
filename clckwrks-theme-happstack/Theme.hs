{-# LANGUAGE FlexibleContexts, OverloadedStrings, RecordWildCards #-}
{-# OPTIONS_GHC -F -pgmFtrhsx #-}
module Theme where

import Clckwrks
import Clckwrks.NavBar.API            (getNavBarData)
import Clckwrks.NavBar.Types          (NavBar(..), NavBarItem(..))
import Clckwrks.ProfileData.Acid      (HasRole(..))
import Data.Maybe                     (fromMaybe)
import qualified Data.Set             as Set
import Data.Text                      (Text, unpack)
import HSP
import Paths_clckwrks_theme_happstack (getDataDir)

------------------------------------------------------------------------------
-- theme
------------------------------------------------------------------------------

theme :: Theme
theme = Theme
    { themeName      = "happstack"
    , _themeTemplate = standardTemplate
    , themeDataDir   = getDataDir
    }

------------------------------------------------------------------------------
-- custom NavBar
------------------------------------------------------------------------------


genNavBar :: GenXML (Clck ClckURL)
genNavBar =
    do menu <- lift getNavBarData
       navBarHTML menu

navBarHTML :: NavBar -> GenXML (Clck ClckURL)
navBarHTML (NavBar menuItems) =
    <div class="navbar navbar-static-full-width">
     <div class="navbar-inner">
      <div class="container">
       <a class="brand" href="/">Happstack</a>
       <div class="nav-collapse">
        <ul class="nav">
         <% mapM mkNavBarItem menuItems %>
        </ul>
       </div>
      </div>
     </div>
    </div>

mkNavBarItem :: NavBarItem -> GenXML (Clck ClckURL)
mkNavBarItem (NBLink (NamedLink ttl lnk)) =
    <li><a href=lnk><% ttl %></a></li>

------------------------------------------------------------------------------
-- standard template
------------------------------------------------------------------------------

standardTemplate :: ( EmbedAsChild (ClckT ClckURL (ServerPartT IO)) headers
                    , EmbedAsChild (ClckT ClckURL (ServerPartT IO)) body
                    ) =>
                    Text
                 -> headers
                 -> body
                 -> XMLGenT (ClckT ClckURL (ServerPartT IO)) XML
standardTemplate ttl hdrs bdy =
    <html>
     <head>
      <title><% ttl %></title>
      <link rel="stylesheet" type="text/css" media="screen" href=(ThemeData "data/css/bootstrap.css")  />
      <link rel="stylesheet" type="text/css" href=(ThemeData "data/css/hscolour.css") />
      <% hdrs %>
      <% googleAnalytics %>
     </head>
     <body>
      <div id="wrap">
       <% genNavBar %>
       <div class="container">
         <div class="row">
          <div class="span8">
           <% bdy %>
          </div>
         </div>
       </div>
       <div id="push"></div>
      </div>

      <footer id="footer" class="footer">
       <div class="container">
         <p class="muted">Powered by <a href="http://happstack.com/">Happstack</a> and <a href="http://clckwrks.com/">clckwrks</a>. Copyright 2013, <a href="http://seereason.com/">SeeReason Partners, LLC</a></p>
       </div>
      </footer>

     </body>
    </html>
