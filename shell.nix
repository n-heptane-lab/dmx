{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, cabal-install, hmidi, mtl, netwire, stdenv, random, serialport, vector, QuickCheck }:
      mkDerivation {
        pname = "dmx";
        version = "0.1.0.0";
        src = ./.;
        libraryHaskellDepends = [ base cabal-install hmidi mtl netwire serialport vector QuickCheck ] ++ (with pkgs.darwin.apple_sdk.frameworks; [ CoreAudio CoreMIDI ]);
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
