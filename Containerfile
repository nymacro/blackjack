# syntax=docker/dockerfile:1

# -----------------------------------------------------------------------------
# Build stage
# -----------------------------------------------------------------------------
FROM docker.io/library/haskell:9.10.3-slim-bookworm AS build

WORKDIR /build

# Build the dependency set first so it can be cached independently of the
# application sources.
COPY stack.yaml stack.yaml.lock mmaker.cabal Setup.hs ./
RUN stack --system-ghc --no-terminal build --only-dependencies

# Build mmaker and place the executable in /out.
COPY LICENSE ./
COPY src ./src
COPY app ./app
COPY tests ./tests
RUN stack --system-ghc --no-terminal build --copy-bins --local-bin-path /out

# -----------------------------------------------------------------------------
# Runtime stage
# -----------------------------------------------------------------------------
FROM docker.io/library/debian:bookworm-slim

RUN apt-get update \
 && apt-get install -y --no-install-recommends \
      libgmp10 \
      libnuma1 \
      zlib1g \
 && rm -rf /var/lib/apt/lists/* \
 && groupadd --gid 10001 mmaker \
 && useradd --uid 10001 --gid mmaker --no-create-home \
      --home-dir /app --shell /usr/sbin/nologin mmaker

WORKDIR /app

COPY --from=build /out/mmaker /usr/local/bin/mmaker
COPY static ./static

USER mmaker

EXPOSE 3000

ENTRYPOINT ["mmaker"]
