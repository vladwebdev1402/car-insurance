FROM haskell:9.8.4

WORKDIR /app

COPY . .

CMD ["stack", "run"]