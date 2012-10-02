{-# OPTIONS_GHC -F -pgmFtrhsx #-}

module Theme.Home where

import Clckwrks
import Theme.Template

summaryBox :: PageId -> String -> String -> GenXML (Clck ClckURL)
summaryBox pid title iconURL =
    <div class="summary-box">
     <h2><% title %></h2>
     <img src=(ThemeData iconURL) />
     <% getPageSummary pid %>
     <p class="read-more"><a href=(ViewPage pid)>read more...</a></p>
    </div>

page :: XMLGenT (Clck ClckURL) XML
page =
    template "happstack.com" () $
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

twitterOld :: XMLGenT (Clck ClckURL) XML
twitterOld =
    <div id="twitter">
      <script type="text/javascript" src="http://widgets.twimg.com/j/2/widget.js"/>
      <script type="text/javascript">
       new TWTR.Widget({
        version: 2,
        type: 'profile',
        rpp: 8,
        interval: 6000,
        width: 'auto',
        height: 300,
        theme: {
          shell: {
            background: '#fff',
            color: '#000'
          },
          tweets: {
            background: '#ffffff',
            color: '#333333',
            links: '#111166'
          }
         },
        features: {
          scrollbar: true,
          loop: false,
          live: false,
          hashtags: true,
          timestamp: true,
          avatars: false,
          behavior: 'all'
        }
       }).render().setUser('happstack').start();
      </script>
  </div>
