FROM haskell:8.10
RUN cabal update
COPY . .
CMD ["cabal", "run"]
EXPOSE 8000
