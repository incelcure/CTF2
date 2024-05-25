push:
  aws --endpoint-url https://s3.backyard-hg.xyz --profile minio-cli s3 \
    cp $(nix build --print-out-paths .#image) s3://registry/ctf

pull:
  aws --endpoint-url https://s3.backyard-hg.xyz --profile minio-cli s3 \
    cp s3://registry/ctf - | docker load

run-container:
  docker run -it $(docker load < $(nix build --print-out-paths .#image) | cut -d' ' -f 3)
