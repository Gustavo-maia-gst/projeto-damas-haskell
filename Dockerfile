FROM haskell:latest

RUN apt-get update --allow-releaseinfo-change &&\
    apt-get install --allow-releaseinfo-change -y libncurses-dev

RUN cabal update

COPY . /app

WORKDIR /app

RUN cabal install