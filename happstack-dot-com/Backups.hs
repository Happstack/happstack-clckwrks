module Main where

import System.Archive.Site (BackupTarget(..), backup)

main = backup (BackupSite {app = "happstack-dot-com-production", user = "upload", host = "happstack.com", keep = 50})
