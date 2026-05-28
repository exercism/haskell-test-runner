ARG HASKELL_VERSION=9.6.7
FROM haskell:${HASKELL_VERSION}-slim-bookworm@sha256:0a066cefb7fa9723242540b141242db9db7a1609131ead85313802ea62d35e20

RUN apt-get update && apt-get install --yes --no-install-recommends jq && rm -rf /var/lib/apt/lists/*

# Set up the environment
ARG HASKELL_VERSION=9.6.7
ENV STACK_ROOT=/opt/test-runner/.stack
ENV LANG=C.UTF-8
ENV PATH=/root/.cabal/bin:/root/.local/bin:/opt/ghc/${HASKELL_VERSION}/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin

WORKDIR /opt/test-runner/

COPY pre-compiled/ .
RUN stack build --resolver lts-22.44 --no-terminal --test --no-run-tests

COPY . .
RUN cd ./test-setup/ && stack build setup-tests --copy-bins --local-bin-path /opt/test-runner/bin/

ENTRYPOINT ["/opt/test-runner/bin/run.sh"]
