(* ::Package:: *)

If[$VersionNumber < 8, 
   Print["SE Uploader requires Mathematica 8 or higher"],

(* get the Installer:*)
   Import["http://packageinstaller.googlecode.com/hg/PackageInstaller/PackageInstaller.m"]; 

(* this is the location of the uploader as advertised here:
   http://meta.mathematica.stackexchange.com/a/32 *)

(* this function will copy the palette locally  to 
   FileNameJoin[{$UserBaseDirectory, "SystemFiles", "FrontEnd", "Palettes", "SE Uploader.nb"}] 
   and open it  *)

    PackageInstaller`InstallPalette["https://raw.github.com/szhorvat/SEUploader/master/SE%20Uploader.nb"];
]
