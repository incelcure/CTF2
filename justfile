push-image:
  docker tag $(docker load < $(nix build --print-out-paths .#image) | cut -d' ' -f 3) localhost:5001/ctf:latest
  docker push localhost:5001/ctf:latest

run-container:
  docker run -it $(docker load < $(nix build --print-out-paths .#image) | cut -d' ' -f 3)
