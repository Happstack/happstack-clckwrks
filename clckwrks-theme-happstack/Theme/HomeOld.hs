{-# OPTIONS_GHC -F -pgmFtrhsx #-}
<%
module Theme.Home where

import Clckwrks

summaryBox :: PageId -> GenXML (Clck ClckURL)
summaryBox pid =
    <div class="summary-box">
     <% getPageSummary pid %>
     <span class="read-more"><a href=(ViewPage pid)>read more...</a></span>
    </div>


page :: XMLGenT (Clck ClckURL) XML

%>

<html>
 <head>
  <title><% getPageTitle %></title>
  <link rel="stylesheet" type="text/css" href=(ThemeData "style.css") />
  <link rel="stylesheet" type="text/css" href=(ThemeData "hscolour.css") />
 </head>
 <body>
  <div id="logo">
   <span>Happstack</span>
  </div>
  <ul id="menu">
    <li><a href="#">Home</a></li>
    <li><a href="#">Documentation</a></li>
    <li><a href="#">FAQ</a></li>
    <li><a href="#">News</a></li>
    <li><a href="#">Support</a></li>
    <li><a href="#">Download</a></li>
    <li><a href="#">Timeline</a></li>
  </ul>

  <div id="background-box">
  </div>

  <div id="banner-box">
   <div class="mesh"></div>
        
   <div class="img-text-bg"></div>
   <div class="img-text">The relentless, uncompromised power and beauty of Haskell for the web</div>
   <img src=(ThemeData "seven.png") />
--   <img src=(ThemeData "lyra.jpg") />
--   <img src=(ThemeData "gears.jpg") />
  </div>

  <blockquote>
   The relentless, uncompromised power and beauty of Haskell for the web
  </blockquote>
    <div class="summary-boxes">
     <div class="summary-box">
      <h2>Elegance</h2>
      <img src=(ThemeData "icons/dowload.png") />
      <p>Lorem ipsum dolor sit amet, consectetur adipiscing elit. Donec ut tortor non augue tincidunt iaculis. Cras ac diam rhoncus nibh commodo iaculis vel sed ligula. Curabitur fringilla tortor sed massa consequat convallis. Maecenas consectetur tincidunt porttitor. Aenean quis posuere augue.</p>
     </div>

     <div class="summary-box">
      <h2>Speed</h2>
      <img src=(ThemeData "icons/phone.png") />
      <p>Lorem ipsum dolor sit amet, consectetur adipiscing elit. Donec ut tortor non augue tincidunt iaculis. Cras ac diam rhoncus nibh commodo iaculis vel sed ligula. Curabitur fringilla tortor sed massa consequat convallis. Maecenas consectetur tincidunt porttitor. Aenean quis posuere augue.</p>
     </div>

     <div class="summary-box">
      <h2>Power</h2>
      <img src=(ThemeData "icons/usb.png") />
      <p>Lorem ipsum dolor sit amet, consectetur adipiscing elit. Donec ut tortor non augue tincidunt iaculis. Cras ac diam rhoncus nibh commodo iaculis vel sed ligula. Curabitur fringilla tortor sed massa consequat convallis. Maecenas consectetur tincidunt porttitor. Aenean quis posuere augue.</p>
     </div>
    </div>

    <div id="footer">
     <div id="copyright">Powered by Happstack. Copyright 2012, Jeremy Shaw</div>
    </div>

 </body>
</html>
