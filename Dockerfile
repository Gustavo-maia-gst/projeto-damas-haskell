FROM haskell:latest

RUN apt-get update &&\
    apt-get install -y libncurses-dev

RUN cabal update

COPY . /app

WORKDIR /app

RUN cabal install
