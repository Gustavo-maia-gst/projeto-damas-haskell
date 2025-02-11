FROM haskell:9.2.8

RUN apt-get update && apt-get install -y libncurses-dev && rm -rf /var/lib/apt/lists/*

RUN cabal update
RUN cabal install --lib hscurses

COPY . /app

WORKDIR /app
