exit # This is not a script, just snippets.

CONTAINER_NAME=serve-rust-to-emacs

docker exec -it $CONTAINER_NAME bash

docker stop $CONTAINER_NAME && docker rm $CONTAINER_NAME

STARTING_AT=$(date)
echo $(date)
docker build -t jeffreybbrown/hode:new .
echo $(date)

DOCKER_IMAGE_SUFFIX="2025-03-13.rust-no-python"
docker tag jeffreybbrown/hode:new jeffreybbrown/hode:latest
docker tag jeffreybbrown/hode:new jeffreybbrown/hode:$DOCKER_IMAGE_SUFFIX
docker rmi jeffreybbrown/hode:new

NATIVE=/home/jeff/hodal/connection-demos/serve-rust-to-emacs
docker run --name $CONTAINER_NAME -it -d \
 -v $NATIVE:/home/ubuntu                 \
 -p 1729:1729                            \
 --platform linux/amd64                  \
  --user 1000:1000                       \
 jeffreybbrown/hode:latest # CAREFUL: :new? :latest?

docker push jeffreybbrown/hode:$DOCKER_IMAGE_SUFFIX
docker push jeffreybbrown/hode:latest
