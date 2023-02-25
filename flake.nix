{
  description = "A purescript todomvc";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";

    ps-tools.follows = "purs-nix/ps-tools";

    purs-nix.url = "github:purs-nix/purs-nix/ps-0.15";
    purs-nix.inputs.nixpkgs.follows = "nixpkgs";

  };

  outputs = inputs@{ self, flake-parts, nixpkgs, ... }:
    flake-parts.lib.mkFlake { inherit self inputs; } {
      systems = [ "x86_64-linux" "x86_64-darwin" ];
      imports = [
        ./nix/purs-nix/flake-module.nix
      ];
      perSystem = { self', config, system, pkgs, lib, ... }: {

        purs-nix = {
          inherit system;
          overlays =
          [
            (self: super:
              let
                build = config.purs-nix-multi.build-local-package;
              in
              {
                client = build self ./src/client;
                server = build self ./src/server;
              }
            )
          ];
        };

        packages = {
          inherit (config.purs-nix.ps-pkgs) client server;
          conduitServer = self'.packages.server.purs-nix-info-extra.ps.module.Main.bundle { };
          conduitClient = self'.packages.client.purs-nix-info-extra.ps.module.Main.bundle { };
        };

        apps =
          let
            nodejsApp = name: script: {
              type = "app";
              program = pkgs.writeShellApplication {
                inherit name;
                text = ''
                  set -x
                  ${lib.getExe pkgs.nodejs} ${script}
                '';
              };
            };
          in
          {
            conduitServer = nodejsApp "conduit-server" self'.packages.conduit-server;
          };

        devShells.default = pkgs.mkShellNoCC {
          name = "conduit-dev-shell";
          packages =
            let
              ps-tools = inputs.purs-nix.inputs.ps-tools.legacyPackages.${system};
            in
            [
              config.purs-nix.purescript
              config.purs-nix-multi.multi-command
              ps-tools.for-0_15.purescript-language-server
              ps-tools.for-0_15.purs-tidy
              pkgs.nixpkgs-fmt
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
