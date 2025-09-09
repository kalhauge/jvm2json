{
  description = "A program for converting bytecode files into json";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/ca77296380960cd497a765102eeb1356eb80fed0";
    flake-utils = {
      url = "github:numtide/flake-utils";
    };
    jvmhs = {
      url = "github:ucla-pls/jvmhs";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    cones = {
      url = "github:kalhauge/cones";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };
  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
      ...
    }@inputs:
    let
      overlays =
        final: prev:
        let
          packages = pfinal: pprev: {
            jvm2json =
              final.haskell.lib.overrideCabal (pfinal.callCabal2nixWithOptions "jvm2json" self "" { })
                (old: {
                  doCheck = false;
                });
            jvmhs = final.haskell.lib.overrideCabal pprev.jvmhs (old: {
              doCheck = false;
            });
          };
        in
        {
          haskellPackages = prev.haskellPackages.extend packages;
          jvm2json = final.haskell.lib.justStaticExecutables final.haskellPackages.jvm2json;
        };
    in
    {
      overlays.default = overlays;
    }
    // flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = inputs.jvmhs.overlays.all ++ [
            inputs.cones.overlays.default
            overlays
          ];
        };
        hpkgs = pkgs.haskellPackages;
      in
      {
        packages = {
          default = pkgs.jvm2json;
          jvm2json = pkgs.jvm2json;
          docker = pkgs.dockerTools.buildLayeredImage {
            name = "jvm2json";
            contents = [ pkgs.jvm2json ];
            config = {
              Cmd = [ "${pkgs.jvm2json}/bin/jvm2json" ];
            };
          };
        };
        devShells =
          let
            buildInputs = with hpkgs; [
              cabal-install
              ghcid
              stack
              haskell-language-server
              hpack
              fourmolu
            ];
            withHoogle = true;
          in
          {
            default = hpkgs.shellFor {
              name = "jvm2json-shell";
              packages = p: [ p.jvm2json ];
              inherit buildInputs withHoogle;
            };
          };
      }
    );
}
