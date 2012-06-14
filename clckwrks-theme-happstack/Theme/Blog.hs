{- | This module defines the 'page' function for rendering an individual blog post or list of blog posts.
-}

{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -F -pgmFtrhsx #-}
module Theme.Blog where

import Clckwrks
import Data.Maybe
import Data.Text (Text, pack, unpack)
import Theme.Template

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
             return $ fromMaybe (pack "Anonymous") mu

page :: XMLGenT (Clck ClckURL) XML
page =
    do ttl <- lift getBlogTitle
       template (unpack ttl) () $
           <%>
            <div id="page-content">
             <h1 class="page-title"><% ttl %></h1>
             <% postsHTML %>
            </div>
           </%>
