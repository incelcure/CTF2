{ pkgs, self', ... }:

let
  python = self'.packages.default.dependencyEnv;
  entrypoint = pkgs.writeShellScriptBin "entrypoint.sh" ''
    python /manage.py makemigrations
    python /manage.py migrate
    python /manage.py collectstatic
    python /manage.py runserver 0.0.0.0:8000
  '';
in

{
  ctf = pkgs.dockerTools.buildImage {
    name = "ctf";

    copyToRoot = [
      ../ctf
      (pkgs.buildEnv {
        name = "image-root";
        paths = with pkgs; [ python entrypoint ];
        pathsToLink = [ "/bin" ];
      })
    ];

    config = {
      Cmd = [ "/bin/entrypoint.sh" ];
      ExposedPorts = { "8000/tcp" = { }; };
    };
  };

  casino = pkgs.dockerTools.buildImage {
    name = "casino";

    copyToRoot = [
      (pkgs.buildEnv {
        name = "image-root";
        paths = with pkgs; [
          (pkgs.haskell.lib.justStaticExecutables self'.packages.casino)
        ];
        pathsToLink = [ "/bin" ];
      })
    ];

    config = {
      Cmd = [ "casino" ];
      ExposedPorts = { "3031/tcp" = { }; };
      Env = [
        "STATIC_PATH=${../casino/static}"
      ];
    };
  };
}
