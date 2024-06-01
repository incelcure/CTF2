{ pkgs, self', ... }:

let
  python = self'.packages.default.dependencyEnv;
  entrypoint = pkgs.writeShellScriptBin "entrypoint.sh" ''
    python /manage.py makemigrations
    python /manage.py migrate
    python /manage.py runserver 0.0.0.0:8000
  '';
in

{
  server = pkgs.dockerTools.buildImage {
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
}
