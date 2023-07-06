{
  description = "A purescript conduit app";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

    flake-parts.url = "github:hercules-ci/flake-parts";

    purs-nix.url = "github:purs-nix/purs-nix/ps-0.15";
    purs-nix.inputs.nixpkgs.follows = "nixpkgs";

    ps-tools.follows = "purs-nix/ps-tools";
  };

  outputs = inputs@{ self, flake-parts, ... }:
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
              slug
              unicode
              uuid
              dotenv
              yoga-om
              js-date
              justifill

              # client
              halogen
              halogen-hooks
              halogen-helix

              # server
              httpurple
              httpurple-yoga-json
              yoga-json
              ansi
              formatters
              stringutils
              crypto
            ];

            dir = ./.;

            test-dependencies = with purs-nix.ps-pkgs; [
              spec
            ];

          };

          conduit-server = pkgs.writeShellScriptBin "conduit-server" ''
            set -x
            find src/Server | entr -s 'echo "compiling..."; purs-nix compile' &
            sleep 1s
            # now start nodemon and send stdin to it
            ${lib.getExe pkgs.nodePackages.nodemon} --watch output --delay 500ms src/Server/main.js <&0
          '';

          watch-compile = pkgs.writeShellScriptBin "watch-compile" ''
            set -x
            find src | entr -s 'echo "compiling..."; purs-nix compile'
          '';

          watch-tests = pkgs.writeShellScriptBin "watch-test" ''
            set -x
            find test src | entr -s 'echo "compiling tests..."; purs-nix test;'
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

                test-module = "Test.Main";
              })
              purs-nix.purescript
              ps-tools.for-0_15.purescript-language-server
              ps-tools.for-0_15.purs-tidy
              conduit-server
              watch-compile
              watch-tests
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
