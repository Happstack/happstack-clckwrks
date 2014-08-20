{-# LANGUAGE OverloadedStrings #-}

import Data.List as List (concat, map)
import Data.Set (singleton)
import Data.Text as T (lines, pack, Text, unlines)
import Debian.AutoBuilder.Details.Atoms (seereasonDefaultAtoms)
import Debian.Debianize (changelog, compat, control, debianization, writeDebianization, doBackups, doWebsite, execMap, inputChangeLog, installTo, missingDependencies, revision, rulesFragments, rulesHead, sourceFormat, tightDependencyFixup, homepage, standardsVersion, evalDebT, newAtoms)
import Debian.Debianize (InstallFile(InstallFile, destDir, destName, execName, sourceDir), Server(..), Site(..))
import Debian.Debianize.Goodies (makeRulesHead)
import Debian.Debianize.Prelude ((~=), (+=), (+++=))
import Debian.Debianize.Types.SourceDebDescription (SourceDebDescription)
import Debian.Policy (databaseDirectory, SourceFormat(Native3), StandardsVersion(StandardsVersion))
import Debian.Pretty (Pretty(pretty))
import Debian.Relation (BinPkgName(BinPkgName), Relation(Rel))
import Distribution.Compiler (CompilerFlavor(GHC))

main :: IO ()
main = newAtoms >>= evalDebT (debianization seereasonDefaultAtoms customize >> writeDebianization)

customize =
    do inputChangeLog
       execMap +++= ("hsx2hs", [[Rel (BinPkgName "hsx2hs") Nothing Nothing]])
       homepage ~= Just "http://www.happstack.com/"
       sourceFormat ~= Just Native3
       missingDependencies += BinPkgName "libghc-clckwrks-theme-happstack-doc"
       revision ~= Just ""
       doWebsite (BinPkgName "happstack-dot-com-production") (theSite (BinPkgName "happstack-dot-com-production"))
       doBackups (BinPkgName "happstack-dot-com-backups") "happstack-dot-com-backups"
       rulesFragments += (pack (Prelude.unlines ["build/happstack-dot-com-production::", "\techo CLCKWRKS=`ghc-pkg field clckwrks version | sed 's/version: //'` > debian/default"]))
       installTo (BinPkgName "happstack-dot-com-production") "debian/default" "/etc/default/happstack-dot-com-production"
       fixRules
       tight
       standardsVersion ~= Just (StandardsVersion 3 9 4 Nothing)
       compat ~= Just 7

serverNames = List.map BinPkgName ["happstack-dot-com-production"]

-- Insert a line just above the debhelper.mk include
fixRules =
    do hd <- makeRulesHead
       rulesHead ~= Just (f hd)
    where
      f t = T.unlines $ List.concat $
            List.map (\ line -> if line == "include /usr/share/cdbs/1/rules/debhelper.mk"
                                then ["DEB_SETUP_GHC_CONFIGURE_ARGS = -fbackups", "", line] :: [T.Text]
                                else [line] :: [T.Text]) (T.lines t)

tight = mapM_ (tightDependencyFixup
                         -- For each pair (A, B) make sure that this package requires the
                         -- same exact version of package B as the version of A currently
                         -- installed during the build.
                         [(BinPkgName "libghc-clckwrks-theme-happstack-dev", BinPkgName "haskell-clckwrks-theme-happstack-utils"),
                          (BinPkgName "libghc-clckwrks-plugin-media-dev", BinPkgName "haskell-clckwrks-plugin-media-utils"),
                          (BinPkgName "libghc-clckwrks-plugin-ircbot-dev", BinPkgName "haskell-clckwrks-plugin-ircbot-utils"),
                          (BinPkgName "libghc-clckwrks-dev", BinPkgName "haskell-clckwrks-utils")]) serverNames

theSite :: BinPkgName -> Site
theSite deb =
    Site { domain = hostname'
         , serverAdmin = "logic@seereason.com"
         , server = theServer deb }

theServer :: BinPkgName -> Server
theServer deb@(BinPkgName _) =
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
                              , "--jstree-path", "/usr/share/clckwrks-$CLCKWRKS/jstree"
                              , "--json2-path", "/usr/share/clckwrks-$CLCKWRKS/json2"
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
