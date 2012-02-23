{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -F -pgmFtrhsx #-}
module Theme.Template where

import Clckwrks
import Data.String (IsString(..))
import Data.Text (Text)
import HSP.Google.Analytics (UACCT)

template ::
    ( EmbedAsChild (Clck ClckURL) headers
    , EmbedAsChild (Clck ClckURL) body
    ) =>
       String
    -> headers
    -> body
    -> XMLGenT (Clck ClckURL) XML
template title headers body =
    <html>
     <head>
      <title><% title %></title>
      <link rel="stylesheet" type="text/css" href=(ThemeData "style.css") />
      <link rel="stylesheet" type="text/css" href=(ThemeData "hscolour.css") />
      <% headers %>
      -- TODO: only show when running on live site, not testing/devel
--      <% analyticsAsync (UACCT "UA-7111625-1") %>
     </head>
     <body>
      <% getMenu %>
      <% body %>

    <div id="footer">
     <div><a href=(Auth $ AuthURL A_Login)>login</a></div>
     <div><a href=(Admin Console)>admin console</a></div>
     <div id="copyright">Powered by Happstack. Copyright 2012, Jeremy Shaw</div>
    </div>
   </body>
  </html>

{-

template ::
    ( EmbedAsChild headers
    , EmbedAsChild body
    ) =>
       String 
    -> headers
    -> body
    -> XMLGenT (Clck ClckURL) XML
template title headers body =
    <html>
     <head>
      <title><% getPageTitle %></title>
      <link rel="stylesheet" type="text/css" href=(ThemeData "style.css")    />
      <link rel="stylesheet" type="text/css" href=(ThemeData "hscolour.css") />
     </head>
     <body>
      <div id="clckwrks-menu">
        <span id="clck"><a href="/">Clck</a></span><span id="wrks"><a href="/">wrks</a></span><br />
        <span id="clckwrks-byline">for secure, reliable, & <br />integrated websites</span>
    --    <% getPageMenu %>
        <% getMenu %>
      </div>

      <div id="clckwrks-body">
       <% body %>

       <p><a href=(Admin Console)>admin</a></p>
       <p><a href=(Auth $ AuthURL A_Login)>login</a></p>

      </div>
     </body>
    </html>


       -}