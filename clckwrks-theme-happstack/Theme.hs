{-# LANGUAGE FlexibleContexts, OverloadedStrings, RecordWildCards #-}
{-# OPTIONS_GHC -F -pgmFtrhsx #-}
module Theme where

import Clckwrks
import Clckwrks.Monad
import Clckwrks.ProfileData.Acid (HasRole(..))
import Data.Maybe                (fromMaybe)
import qualified Data.Set        as Set
import Data.Text                 (Text, unpack)
-- import Happstack.Server
import HSP
import Paths_clckwrks_theme_happstack (getDataDir)

theme :: Theme
theme = Theme
    { themeName      = "happstack"
    , _themeTemplate = pageTemplate
    , themeBlog      = blog
    , themeDataDir   = getDataDir
    }

pageTemplate :: ( EmbedAsChild (ClckT ClckURL (ServerPartT IO)) headers
                , EmbedAsChild (ClckT ClckURL (ServerPartT IO)) body
                ) =>
                Text
             -> headers
             -> body
             -> XMLGenT (ClckT ClckURL (ServerPartT IO)) XML
pageTemplate ttl hdr bdy =
    do pid <- XMLGenT $ getPageId
       case pid of
         (PageId 1) -> home ttl hdr bdy
         _          -> standardTemplate ttl hdr <div id="page-content">
                                                 <h1 class="page-title"><% ttl %></h1>
                                                 <% bdy %>
                                                </div>

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
      <link rel="stylesheet" type="text/css" href=(ThemeData "style.css") />
      <link rel="stylesheet" type="text/css" href=(ThemeData "hscolour.css") />
      <% hdrs %>
      <% googleAnalytics %>
     </head>
     <body>
      <div class="page-menu">
       <span id="logo">Happstack</span>
       <div class="menu-inner-div">
        <% getMenu %>
       </div>
      </div>

      <% bdy %>

      <div id="footer">
       <% do mu <- getUserId
             case mu of
               Nothing -> <% () %>
               (Just uid) ->
                   do r <- query (HasRole uid (Set.singleton Administrator))
                      if not r
                        then <% () %>
                        else do pid <- lift getPageId
                                <%>
                                 <div><a href=(Auth $ AuthURL A_Login)>login</a></div>
                                 <div><a href=(Admin Console)>admin console</a></div>
                                 <div><a href=(Admin (EditPage pid))>edit this page</a></div>
                                </%>
         %>
       <div id="copyright">Powered by Happstack. Copyright 2012, SeeReason Partners LLC</div>
      </div>
     </body>
    </html>

------------------------------------------------------------------------------
-- Home
------------------------------------------------------------------------------

summaryBox :: PageId -> String -> String -> GenXML (Clck ClckURL)
summaryBox pid title iconURL =
    <div class="summary-box">
     <h2><% title %></h2>
     <img src=(ThemeData iconURL) />
     <% getPageSummary pid %>
     <p class="read-more"><a href=(ViewPage pid)>read more...</a></p>
    </div>


home ttl hdr bdy =
    standardTemplate "happstack.com" hdr $
        <%>
         <% twitter %>
         <div id="banner-box">
          <div class="mesh"></div>

          <div class="img-text-bg"></div>
          <div class="img-text">The relentless, uncompromised power and beauty of Haskell in a web framework.</div>

          <img src=(ThemeData "seven-black.png") />

         </div>

         <blockquote>
          <p>The relentless, uncompromised power and beauty of Haskell in a web framework.</p>
         </blockquote>

         <% bdy %>

         <div class="summary-boxes">
          <% summaryBox (PageId 5) "Happstack Philosophy" "philosophy-icon.png" %>
          <% summaryBox (PageId 6) "Happstack 7 Release Notes" "7-icon.png" %>
          <% summaryBox (PageId 7) "Happstack 8 Roadmap" "8-icon.png" %>

         </div>

       </%>

twitter =
   <div id="twitter">
    <a class="twitter-timeline" data-dnt="true" href="https://twitter.com/happstack" data-widget-id="253241954832367616">Tweets by @happstack</a>
    <script>!function(d,s,id){var js,fjs=d.getElementsByTagName(s)[0];if(!d.getElementById(id)){js=d.createElement(s);js.id=id;js.src="//platform.twitter.com/widgets.js";fjs.parentNode.insertBefore(js,fjs);}}(document,"script","twitter-wjs");</script>
   </div>


------------------------------------------------------------------------------
-- Blog
------------------------------------------------------------------------------

-- | create a list of of all the blog posts
postsHTML :: XMLGenT (Clck ClckURL) XML
postsHTML =
    do posts <- getPosts
       <ol class="blog-posts">
        <% mapM postHTML posts %>
        </ol>

-- | create a the \<li\> for a single blog post
postHTML :: Page -> XMLGenT (Clck ClckURL) XML
postHTML Page{..} =
    <li class="blog-post">
     <h1><% pageTitle %></h1>
     <span class="pub-info">Posted on <span class="pub-date"><% pageDate %></span> by <span class="author"><% authorName %></span></span>
     <% pageSrc %>
     <p><a href=(ViewPage pageId)>permalink</a></p>
    </li>
    where
      authorName :: Clck ClckURL Text
      authorName =
          do mu <- getUsername pageAuthor
             return $ fromMaybe "Anonymous" mu

blog :: XMLGenT (Clck ClckURL) XML
blog =
    do ttl <- lift getBlogTitle
       standardTemplate ttl () $
           <%>
            <div id="page-content">
             <h1 class="page-title"><% ttl %></h1>
             <% postsHTML %>
            </div>
           </%>
