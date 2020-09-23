{
  description = "beam flake";

  inputs.flake-utils.url = github:numtide/flake-utils;
  inputs.euler-build.url = flake:euler-build/wip;

  outputs = { self, euler-build, flake-utils }:
    {
      overlay = import ./nix/overlay.nix;
    } //
    (flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import euler-build.nixpkgs {
          inherit system;
          overlays = [ euler-build.overlay self.overlay ];
        };
      in
        {
          packages = with pkgs.eulerHaskellPackages; {
           inherit beam-core;
           inherit beam-migrate;
           inherit beam-migrate-cli;
           inherit beam-postgres;
           inherit beam-sqlite;
          };

          defaultPackage = pkgs.eulerHaskellPackages.beam-core;
        }
    ));
}
