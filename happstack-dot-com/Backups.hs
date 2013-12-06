module Main where

import Network.URI (URIAuth(..))
import System.Archive.Site (BackupTarget(..), backup)

main = backup (BackupTarget
               { app = "happstack-dot-com-production"
               , auth = URIAuth { uriUserInfo = "upload@"
                                , uriRegName = "happstack.com"
                                , uriPort = "" }
               , keep = 50, delay = 0
               , localTop = "/srv/backups"
               , remoteTop = "/srv"
               , nice = 10
               , bwLimit = Just 20 })
