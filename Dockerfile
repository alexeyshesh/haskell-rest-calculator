FROM haskell:8

RUN mkdir /app
ADD . /app
WORKDIR /app

RUN cabal update
RUN cabal build

CMD cabal run
