{
  description = "A purescript todomvc";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

    flake-parts.url = "github:hercules-ci/flake-parts";

    purs-nix.url = "github:purs-nix/purs-nix/ps-0.15";
    purs-nix.inputs.nixpkgs.follows = "nixpkgs";

    ps-tools.follows = "purs-nix/ps-tools";
  };

  outputs = inputs@{ self, flake-parts, nixpkgs, ... }:
    flake-parts.lib.mkFlake { inherit self inputs; } {
      systems = [ "x86_64-linux" "x86_64-darwin" ];
      imports = [ ];
      perSystem = { self', config, system, pkgs, lib, ... }:
        let
          ps-tools = inputs.ps-tools.legacyPackages.${system};

          purs-nix = inputs.purs-nix {
            inherit system;
          };

          ps = purs-nix.purs {
            dependencies = with purs-nix.ps-pkgs; [
              console
              effect
              prelude

              # client
              halogen
              halogen-hooks
              halogen-helix

              # server
              httpurple
            ];

            dir = ./.;
          };
        in
        {

          formatter = pkgs.nixpkgs-fmt;

          devShells.default = pkgs.mkShell {
            name = "conduit";
            packages = with pkgs; [
              nodejs
              (ps.command {
                bundle = {
                  esbuild = {
                    outfile = "public/bundle.js";
                  };
                  module = "Client.Main";
                };
              })
              purs-nix.purescript
              ps-tools.for-0_15.purescript-language-server
              ps-tools.for-0_15.purs-tidy
            ];
            shellHook = ''
              export NIX_SHELL_NAME="conduit-dev"
              zsh
              exit 0
            '';
          };
        };
    };
}
