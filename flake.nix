{
  description = "beam flake";

  inputs.flake-utils.url = github:numtide/flake-utils;
  inputs.euler-build.url = flake:euler-build/wip;
  inputs.euler-build.inputs.flake-utils.follows = "flake-utils";

  # outputs = { self, euler-build, flake-utils }:
  #   {
  #     overlay = import ./nix/overlay.nix;
  #   } //
  #   (flake-utils.lib.eachDefaultSystem (system:
  #     let
  #       pkgs = import euler-build.nixpkgs {
  #         inherit system;
  #         overlays = [ euler-build.overlay self.overlay ];
  #       };
  #     in
  #       {
  #         packages = with pkgs.eulerHaskellPackages; {
  #          inherit beam-core;
  #          inherit beam-migrate;
  #          inherit beam-migrate-cli;
  #          inherit beam-postgres;
  #          inherit beam-sqlite;
  #          inherit interpolate;
  #         };

  #         defaultPackage = pkgs.eulerHaskellPackages.beam-core;
  #       }
  #   ));

  outputs = flakeInputs@{ self, ... }:
    flakeInputs.euler-build.mkEulerFlake {
      flakeName = "beam";
      defaultPackageName = "beam-core";
      exportPackages = [
        "beam-core"
        "beam-migrate"
        "beam-migrate-cli"
        "beam-postgres"
        "beam-sqlite"
      ];
      overlayPath = ./nix/overlay.nix;
      shellToolsPath = ./shell-tools.nix;
      inputs = flakeInputs;
    };
}
