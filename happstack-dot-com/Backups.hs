module Main where

import System.Archive.Site (BackupTarget(..), backup)

main = backup (BackupTarget {app = "happstack-dot-com-production", user = "upload", host = "happstack.com", keep = 50})
