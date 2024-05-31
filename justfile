push-image:
  docker tag $(docker load < $(nix build --print-out-paths .#image) | cut -d' ' -f 3) localhost:5001/ctf:latest
  docker push localhost:5001/ctf:latest

run-container:
  docker run -it $(docker load < $(nix build --print-out-paths .#image) | cut -d' ' -f 3)

encrypt:
  for f in $(ls infra/*-secret.yaml); do gpg --encrypt --recipient trashbin2019np@gmail.com --recipient blueberry --recipient cherry --batch --yes $f; done

decrypt:
  for f in $(ls infra/*-secret.yaml.gpg | sed -e 's/\.gpg//'); do gpg --decrypt --batch --yes --output $f $f.gpg; done
