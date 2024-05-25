{
  inputs = {
    flake-parts.url = "github:hercules-ci/flake-parts";
    poetry2nix.url = "github:nix-community/poetry2nix";
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  };

  outputs = inputs@{ flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      imports = [
      ];
      systems = [ "x86_64-linux" "aarch64-linux" "aarch64-darwin" "x86_64-darwin" ];
      perSystem = { config, self', inputs', pkgs, system, ... }:
        let
          p2n = inputs.poetry2nix.lib.mkPoetry2Nix { inherit pkgs; };
          projectDir = ./.;
          overrides = p2n.defaultPoetryOverrides.extend
            (final: prev: {
              postgres = prev.postgres.overridePythonAttrs (old: {
                buildInputs = (old.buildInputs or [ ]) ++ [ prev.setuptools ];
              });
              psycopg2-pool = prev.psycopg2-pool.overridePythonAttrs (old: {
                buildInputs = (old.buildInputs or [ ]) ++ [ prev.setuptools ];
              });
            });
        in
        {
          packages.default = p2n.mkPoetryApplication { inherit overrides projectDir; };

          devShells.default = pkgs.mkShellNoCC {
            packages = with pkgs; [
              (p2n.mkPoetryEnv { inherit overrides projectDir; })
              poetry
            ];
          };
        };
      flake = { };
    };
}
