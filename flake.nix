{
  description = "beam flake";

  inputs.flake-utils.url = github:numtide/flake-utils;
  inputs.euler-build.url = flake:euler-build/wip;
  inputs.euler-build.inputs.flake-utils.follows = "flake-utils";

  outputs = flakeInputs@{ self, ... }:
    flakeInputs.euler-build.mkEulerFlake {
      overlayPath = ./nix/overlay.nix;
      configPath = ./nix/confix.nix;
      inputs = flakeInputs;
    };
}
