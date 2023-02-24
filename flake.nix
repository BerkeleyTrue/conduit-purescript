{
  description = "A purescript todomvc";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

    ps-tools.follows = "purs-nix/ps-tools";

    purs-nix.url = "github:purs-nix/purs-nix/ps-0.15";

    utils.url = "github:numtide/flake-utils";
  };

  outputs = { nixpkgs, utils, ... }@inputs:
    utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" ]
      (system:
        let
          pkgs = import nixpkgs {
            inherit system;
          };

          ps-tools = inputs.ps-tools.legacyPackages.${system};
          purs-nix = inputs.purs-nix {
            inherit system;
          };
          ps =
            purs-nix.purs
              {
                dependencies =
                  with purs-nix.ps-pkgs;
                  [
                    console
                    effect
                    prelude
                    halogen
                    halogen-hooks
                    halogen-helix
                  ];

                dir = ./.;
              };
          pursShell = pkgs.mkShell
            {
              packages =
                with pkgs;
                [
                  entr
                  nodejs
                  (ps.command {
                    bundle = {
                      esbuild = {
                        outfile = "./public/bundle.js";
                      };
                    };
                  })
                  ps-tools.for-0_15.purescript-language-server
                  purs-nix.esbuild
                  purs-nix.purescript
                  nodePackages.purs-tidy
                ];

              shellHook = ''
                zsh
                exit 0
              '';
            };
        in
        {
          packages.default = ps.bundle { };
          formatter = pkgs.nixpkgs-fmt;
          devShells.default = pursShell;

        });
}
