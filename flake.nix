{
  description = "A program for converting bytecode files into json";

  inputs =
    {
      nixpkgs.url = "github:NixOS/nixpkgs/23.05";
      flake-utils.url = "github:numtide/flake-utils";
      jvmhs.url = "github:ucla-pls/jvmhs?ref=zipless";
      cones.url = "github:kalhauge/cones";
    };
  outputs = { self, nixpkgs, flake-utils, ... }@inputs:
    let
      overlays = final: prev:
        let
          packages = p: {
            "jvm2json" = final.haskell.lib.overrideCabal (p.callCabal2nixWithOptions "jvm2json" self "" { })
              (old: {
                doCheck = false;
              });
          };
        in
        {
          haskellPackages = prev.haskellPackages.extend (p: _: packages p);
          jvm2json = final.haskell.lib.justStaticExecutables final.haskellPackages.jvm2json;
        };
    in
    {
      overlays.default = overlays;
    } // flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs
          {
            inherit system;
            overlays = inputs.jvmhs.overlays.all ++ [ inputs.cones.overlays.default overlays ];
          };
        hpkgs = pkgs.haskellPackages;
      in
      {
        packages = {
          default = pkgs.jvm2json;
          jvm2json = pkgs.jvm2json;
          docker = pkgs.dockerTools.buildLayeredImage
            {
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
              haskell-language-server
              hpack
              fourmolu
            ];
            withHoogle = true;
          in
          {
            default = hpkgs.shellFor
              {
                name = "jvm2json-shell";
                packages = p: [ p.jvm2json ];
                inherit buildInputs withHoogle;
              };
          };
      });
}
