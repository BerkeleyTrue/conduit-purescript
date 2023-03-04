{
  description = "A purescript conduit app";

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
      imports = [
        inputs.flake-parts.flakeModules.easyOverlay
      ];
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
              ansi
              formatters
              stringutils
            ];

            dir = ./.;
          };

          conduit-server = pkgs.writeShellScriptBin "conduit-server" ''
            set -x
            find src/Server | entr -s 'echo "compiling..."; purs-nix compile' &
            sleep 1s
            ${lib.getExe pkgs.nodePackages.nodemon} --watch output --delay 500ms src/Server/main.js
          '';
        in
        {

          overlayAttrs = {
            inherit conduit-server;
          };

          formatter = pkgs.nixpkgs-fmt;

          devShells.default = pkgs.mkShell {
            name = "conduit";

            packages = with pkgs; [
              entr
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
              conduit-server
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
