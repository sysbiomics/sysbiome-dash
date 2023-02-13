TAG=$(git tag -l --sort=-creatordate | head -n 1)
# docker build . -f docker/base.Dockerfile -t yumyai/sysmiome-dash_base:$TAG --progress=plain  |& tee build.log
docker build . -f docker/build.Dockerfile -t yumyai/sysmiome-dash:$TAG --progress=plain
