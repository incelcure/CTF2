{pkgs, self', ...}:

let
  python = "${self'.packages.default.dependencyEnv}/bin/python";
  entrypoint = pkgs.writeShellScriptBin "entrypoint.sh" ''
    ${python} ${../ctf}/manage.py makemigrations
    ${python} ${../ctf}/manage.py migrate
    ${python} ${../ctf}/manage.py runserver 0.0.0.0:8000
  '';
in

pkgs.dockerTools.buildImage {
  name = "ctf";

  copyToRoot = pkgs.buildEnv {
    name = "image-root";
    paths = with pkgs; [ entrypoint ];
    pathsToLink = [ "/bin" ];
  };

  config = {
    Cmd = [ "/bin/entrypoint.sh" ];
    ExposedPorts = { "8000/tcp" = { }; };
  };
}
