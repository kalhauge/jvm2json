{
  description = "A program for converting bytecode files into json";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/ca77296380960cd497a765102eeb1356eb80fed0";
    jvmhs = {
      url = "github:ucla-pls/jvmhs";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    cones = {
      url = "github:kalhauge/cones";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };
  outputs = {
    self,
    nixpkgs,
    flake-utils,
    ...
  } @ inputs: let
    overlay = final: prev: let
      packages = pfinal: pprev: {
        jvm2json =
          final.haskell.lib.overrideCabal (pfinal.callCabal2nixWithOptions "jvm2json" self "" {})
          (old: {
            doCheck = false;
          });
        jvmhs = final.haskell.lib.overrideCabal pprev.jvmhs (old: {
          doCheck = false;
        });
      };
    in {
      haskellPackages = prev.haskellPackages.extend packages;
      jvm2json = final.haskell.lib.justStaticExecutables final.haskellPackages.jvm2json;
    };
    perSystem = {
      systems ? [
        "x86_64-linux"
        "x86_64-darwin"
      ],
      do,
    }:
      nixpkgs.lib.genAttrs systems (
        system:
          do {
            inherit system;
            pkgs = import nixpkgs {
              inherit system;
              overlays =
                inputs.jvmhs.overlays.all
                ++ [
                  overlay
                  inputs.cones.overlays.default
                ];
            };
          }
      );
  in {
    packages = perSystem {
      do = {pkgs, ...}: rec {
        default = pkgs.jvm2json;
        jvm2json = pkgs.jvm2json;
        dockers =
          pkgs.linkFarm "dockers"
          {
            "jvm2json.tar.gz" = docker;
            "jvm2json-jdk.tar.gz" = docker-jdk;
          };
        docker = pkgs.dockerTools.buildImage {
          name = "jvm2json";
          tag = "latest";
          config = {
            Cmd = ["${pkgs.jvm2json}/bin/jvm2json"];
          };
        };
        docker-jdk = pkgs.dockerTools.buildImage {
          name = "jvm2json";
          tag = "jdk";

          copyToRoot = pkgs.buildEnv {
            name = "jvm2json-jdk";
            paths = [
              pkgs.bashInteractive
              pkgs.coreutils
              pkgs.jdk
              pkgs.jvm2json
            ];
          };

          config = {
            Cmd = ["/bin/bash"];
            WorkingDir = "/workspace";
            Env = [
              "JAVA_HOME=${pkgs.jdk}"
            ];
          };
        };
      };
    };
    devShells = perSystem {
      do = {pkgs, ...}: let
        hpkgs = pkgs.haskellPackages;
        buildInputs = with hpkgs; [
          cabal-install
          ghcid
          stack
          haskell-language-server
          hpack
          fourmolu
        ];
        withHoogle = true;
      in {
        default = hpkgs.shellFor {
          name = "jvm2json-shell";
          packages = p: [p.jvm2json];
          inherit buildInputs withHoogle;
        };
      };
    };
  };
}
