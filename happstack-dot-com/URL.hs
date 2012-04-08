{-# LANGUAGE TemplateHaskell #-}
module URL where

import Clckwrks.URL    (ClckURL)
import Clckwrks.Media  (MediaURL)
import Clckwrks.IrcBot (IrcBotURL)
import Web.Routes.TH   (derivePathInfo)

data SiteURL
    = C ClckURL
    | M MediaURL
    | I IrcBotURL
$(derivePathInfo ''SiteURL)
