{
  inputs = {
    flake-parts.url = "github:hercules-ci/flake-parts";
    poetry2nix.url = "github:nix-community/poetry2nix";
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    haskell-flake.url = "github:srid/haskell-flake";
    treefmt-nix.url = "github:numtide/treefmt-nix";
    treefmt-nix.inputs.nixpkgs.follows = "nixpkgs";
    fourmolu-nix.url = "github:jedimahdi/fourmolu-nix";
  };

  outputs = inputs@{ flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {

      imports = [
        inputs.haskell-flake.flakeModule
        inputs.treefmt-nix.flakeModule
        inputs.fourmolu-nix.flakeModule
      ];

      systems = [ "x86_64-linux" "aarch64-linux" "aarch64-darwin" "x86_64-darwin" ];

      perSystem = args@{ config, self', inputs', lib, pkgs, system, ... }:
        let

          p2n = inputs.poetry2nix.lib.mkPoetry2Nix { inherit pkgs; };

          overrides = p2n.defaultPoetryOverrides.extend
            (final: prev: {
              postgres = prev.postgres.overridePythonAttrs (old: {
                buildInputs = (old.buildInputs or [ ]) ++ [ prev.setuptools ];
              });
              psycopg2-pool = prev.psycopg2-pool.overridePythonAttrs (old: {
                buildInputs = (old.buildInputs or [ ]) ++ [ prev.setuptools ];
              });
            });

          p2n-args = {
            inherit overrides;
            projectDir = lib.sourceByRegex
              ./.
              [ "ctf.*" "poetry.lock" "pyproject.toml" "README.md" ];
          };

        in
        {

          haskellProjects.default = {
            projectRoot = ./casino;

            devShell = {
              hlsCheck.enable = false;
            };

            autoWire = [ "packages" "apps" "checks" ];
          };

          packages.default = p2n.mkPoetryApplication p2n-args;
          packages.env = self'.packages.default.dependencyEnv;

          packages.image = (import ./nix/image.nix args).ctf;
          packages.image-casino = (import ./nix/image.nix args).casino;

          treefmt.config = {
            projectRootFile = "flake.nix";

            programs.fourmolu.enable = true;
            programs.fourmolu.package = config.fourmolu.wrapper;
            programs.cabal-fmt.enable = true;

            settings.formatter.hlint = {
              command = pkgs.hlint;
              options = [ "-j" "-X" "QuasiQuotes" "-X" "TemplateHaskell" ];
              includes = [ "*.hs" ];
            };
          };

          fourmolu.settings = {
            indentation = 2;
            comma-style = "leading";
            column-limit = 80;
            record-brace-space = true;
            indent-wheres = true;
            import-export-style = "diff-friendly";
            respectful = true;
            haddock-style = "multi-line";
            newlines-between-decls = 1;
            extensions = [ "ImportQualifiedPost" ];
          };

          devShells.default = pkgs.mkShell {
            inputsFrom = [
              config.haskellProjects.default.outputs.devShell
              config.treefmt.build.devShell
            ];

            packages = with pkgs; [
              (p2n.mkPoetryEnv p2n-args)
              just
              poetry
              kubectl
              yaml-language-server
            ];
          };
        };

      flake = {
        hydraJobs.build-image = inputs.self.packages.x86_64-linux.server;
      };
    };
}
