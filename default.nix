{ pkgs ? import <nixpkgs> {} }:

let
  drv = pkgs.haskellPackages.callPackage ./duckloader.nix {};
in
  drv
