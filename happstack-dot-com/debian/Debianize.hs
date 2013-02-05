{-# LANGUAGE OverloadedStrings #-}
import Data.Lens.Lazy (setL, modL)
import Data.List as List (map, isPrefixOf, concat, foldr)
import Data.Maybe (fromMaybe)
import Data.Text as T
import Debian.Changes (ChangeLog)
import Debian.Debianize
import Debian.Relation (Relation(..), VersionReq(..), SrcPkgName(..), BinPkgName(..))
import qualified Paths_clckwrks as Clckwrks
import Text.PrettyPrint.ANSI.Leijen (Pretty, pretty, text)

main :: IO ()
main =
    do -- old <- inputDebianization "."
       jstreePath <- Clckwrks.getDataFileName "jstree"
       json2Path  <- Clckwrks.getDataFileName "json2"
       log <- inputChangeLog "debian"
       getSimplePackageDescription' "." defaultAtoms >>=
         cabalToDebianization "." . customize jstreePath json2Path log >>=
         -- putStrLn . describeDebianization old
         writeDebianization

customize jstreePath json2Path log =
    modControl (\ y -> y {homepage = Just "http://www.happstack.com/"}) .
    setL sourceFormat (Just Native3) .
    missingDependency (BinPkgName "libghc-clckwrks-theme-happstack-doc") .
    setRevision "" .
    doWebsite (BinPkgName "happstack-dot-com-production") (theSite jstreePath json2Path (BinPkgName "happstack-dot-com-production")) .
    doBackups (BinPkgName "happstack-dot-com-backups") "happstack-dot-com-backups" .
    fixRules .
    tight .
    setL changelog (Just log) .
    modControl (\ x -> x {standardsVersion = Just (StandardsVersion 3 9 4 Nothing)}) .
    setL compat (Just 7)

serverNames = List.map BinPkgName ["happstack-dot-com-production"]

-- Insert a line just above the debhelper.mk include
fixRules deb =
    modL rulesHead (\ mt -> (Just . f) (fromMaybe (getRulesHead deb) mt)) deb
    where
      f t = T.unlines $ List.concat $
            List.map (\ line -> if line == "include /usr/share/cdbs/1/rules/debhelper.mk"
                                then ["DEB_SETUP_GHC_CONFIGURE_ARGS = -fbackups", "", line] :: [T.Text]
                                else [line] :: [T.Text]) (T.lines t)

tight deb = List.foldr (tightDependencyFixup
                         -- For each pair (A, B) make sure that this package requires the
                         -- same exact version of package B as the version of A currently
                         -- installed during the build.
                         [(BinPkgName "libghc-clckwrks-theme-happstack-dev", BinPkgName "haskell-clckwrks-theme-happstack-utils"),
                          (BinPkgName "libghc-clckwrks-plugin-media-dev", BinPkgName "haskell-clckwrks-plugin-media-utils"),
                          (BinPkgName "libghc-clckwrks-plugin-ircbot-dev", BinPkgName "haskell-clckwrks-plugin-ircbot-utils"),
                          (BinPkgName "libghc-clckwrks-dev", BinPkgName "haskell-clckwrks-utils")]) deb serverNames

theSite :: FilePath -> FilePath -> BinPkgName -> Site
theSite jstreePath json2Path deb =
    Site { domain = hostname'
         , serverAdmin = "logic@seereason.com"
         , server = theServer jstreePath json2Path deb }

theServer :: FilePath -> FilePath -> BinPkgName -> Server
theServer jstreePath json2Path deb@(BinPkgName _) =
    Server { hostname = hostname'
           , port = portNum deb
           , headerMessage = "Generated by happstack-dot-com/Setup.hs"
           , retry = "60"
           , serverFlags =    [ "--http-port", show (portNum deb)
                              , "--hide-port"
                              , "--hostname", hostname'
                              , "--top", databaseDirectory deb
                              , "--enable-analytics"
                              , "--jquery-path", "/usr/share/javascript/jquery/"
                              , "--jqueryui-path", "/usr/share/javascript/jquery-ui/"
                              , "--jstree-path", jstreePath
                              , "--json2-path",json2Path
                              ]
           , installFile =
                     InstallFile { execName   = "happstack-dot-com-server"
                                 , destName   = show (pretty deb)
                                 , sourceDir  = Nothing
                                 , destDir    = Nothing }
           }

hostname' = "happstack.com"

portNum (BinPkgName deb) =
    case deb of
            "happstack-dot-com-production"  -> 9028
--            "happstack-dot-com-staging"     -> 9038
--            "happstack-dot-com-development" -> 9039
            _ -> error $ "Unexpected package name: " ++ deb
