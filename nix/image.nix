{pkgs, self', ...}:

pkgs.dockerTools.buildImage {
  name = "ctf";

  # copyToRoot = pkgs.buildEnv {
  #   name = "image-root";
  #   paths = with pkgs; [ ];
  #   pathsToLink = [ "/bin" ];
  # };

  config = {
    Cmd = [
      "${self'.packages.default.dependencyEnv}/bin/python"
      "${../ctf}/manage.py"
      "runserver"
      "0.0.0.0:8000"
    ];
    ExposedPorts = { "8000/tcp" = { }; };
    # ExtraCommands = '' '';
  };
}
