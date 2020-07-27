FROM haskell:8.8
RUN cabal update
RUN cabal install brok
CMD ["brok"]
