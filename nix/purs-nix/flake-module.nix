{ self, inputs, ... }: {
  perSystem = { config, self', inputs', system, pkgs, lib, ... }: {
    options = {
      purs-nix = lib.mkOption {
        type = lib.types.unspecified;
      };

      purs-nix-multi = lib.mkOption {
        description = ''
          An example of purescript front end and back end using purs-nix-multi.
        '';

        type = lib.types.attrsOf lib.types.unspecified;

        default = import ./multi.nix {
          inherit self pkgs lib inputs;
          inherit (config) purs-nix;
        };
      };
    };
  };
}
