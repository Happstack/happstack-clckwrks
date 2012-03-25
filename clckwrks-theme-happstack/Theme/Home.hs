{-# OPTIONS_GHC -F -pgmFtrhsx #-}

module Theme.Home where

import Clckwrks
import Theme.Template

summaryBox :: PageId -> GenXML (Clck ClckURL)
summaryBox pid =
    <div class="summary-box">
     <% getPageSummary pid %>
     <span class="read-more"><a href=(ViewPage pid)>read more...</a></span>
    </div>


page :: XMLGenT (Clck ClckURL) XML
page =
    template "Home" () $
        <%>
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
          <div class="summary-box">
           <h2>Scalable</h2>
           <img src=(ThemeData "icons/scalable.jpg") />
           <p>Lorem ipsum dolor sit amet, consectetur adipiscing elit. Donec ut tortor non augue tincidunt iaculis. Cras ac diam rhoncus nibh commodo iaculis vel sed ligula. Curabitur fringilla tortor sed massa consequat convallis. Maecenas consectetur tincidunt porttitor. Aenean quis posuere augue.</p>
          </div>

          <div class="summary-box">
           <h2>Reliable</h2>
           <img src=(ThemeData "icons/reliable.jpg") />
           <p>Lorem ipsum dolor sit amet, consectetur adipiscing elit. Donec ut tortor non augue tincidunt iaculis. Cras ac diam rhoncus nibh commodo iaculis vel sed ligula. Curabitur fringilla tortor sed massa consequat convallis. Maecenas consectetur tincidunt porttitor. Aenean quis posuere augue.</p>
          </div>

          <div class="summary-box">
           <h2>Elegant</h2>
           <img src=(ThemeData "icons/elegant.jpg") />
           <p>Lorem ipsum dolor sit amet, consectetur adipiscing elit. Donec ut tortor non augue tincidunt iaculis. Cras ac diam rhoncus nibh commodo iaculis vel sed ligula. Curabitur fringilla tortor sed massa consequat convallis. Maecenas consectetur tincidunt porttitor. Aenean quis posuere augue.</p>
          </div>
         </div>

       </%>
       
