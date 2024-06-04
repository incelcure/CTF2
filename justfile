default:
    @just --list

push-image target="ctf":
  docker tag $(docker load < $(nix build --print-out-paths .#image-{{target}}) | cut -d' ' -f 3) localhost:5001/{{target}}:latest
  docker push localhost:5001/{{target}}:latest

run-container target="ctf":
  docker run -it $(docker load < $(nix build --print-out-paths .#image-{{target}}) | cut -d' ' -f 3)

set-registry-addr reg:
  for f in $(rg -l 'localhost:5001/'); do sed -i $f -e 's,localhost:5001/,{{reg}},g'; done

encrypt:
  for f in $(ls infra/*-secret.yaml); do gpg --encrypt --recipient trashbin2019np@gmail.com --recipient blueberry --recipient cherry --batch --yes $f; done

decrypt:
  for f in $(ls infra/*-secret.yaml.gpg | sed -e 's/\.gpg//'); do gpg -q --decrypt --batch --yes --output $f $f.gpg; done

apply-secrets:
  for f in $(ls infra/*-secret.yaml.gpg); do gpg -q --decrypt --batch $f | kubectl apply -f -; done
