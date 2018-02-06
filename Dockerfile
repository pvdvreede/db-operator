FROM haskell:8
WORKDIR /app
RUN cabal update
COPY db-operator.cabal /app/
RUN cabal install -j4 --dependencies-only
