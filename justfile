run-container:
  docker run -it $(docker load < $(nix build --print-out-paths .#image) | cut -d' ' -f 3)
