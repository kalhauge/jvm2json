{
  description = "A program for converting bytecode files into json";

  inputs =
    {
      nixpkgs.url = "github:NixOS/nixpkgs/23.05";
      flake-utils.url = "github:numtide/flake-utils";
      jvmhs.url = "github:ucla-pls/jvmhs?ref=zipless";
    };
  outputs = { self, nixpkgs, flake-utils, ... }@inputs:
    let
      packages = p: {
        "jvm2json" = p.callCabal2nixWithOptions "jvmhs" self "" { };
      };
      overlays = final: prev: {
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
            overlays = [ overlays ] ++ inputs.jvmhs.overlays.all;
          };
        hpkgs = pkgs.haskellPackages;
      in
      {
        packages = {
          default = hpkgs.jvm2json;
          jvm2json = pkgs.jvm2json;
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
                name = "jvmhs-shell";
                packages = p: [ p.jvm2json ];
                inherit buildInputs withHoogle;
              };
          };
      });
}
