FROM haskell:8.10
RUN cabal update
CMD ["cabal", "run"]
EXPOSE 8000
