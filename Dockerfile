# ---- builder: the official Haskell image; compiles the dependency snapshot ----
# These build instructions are kept identical to the historical single-stage
# build so the expensive `stack build` layer is reused from the cache.
ARG HASKELL_VERSION=9.6.7
FROM haskell:${HASKELL_VERSION}-slim-bookworm@sha256:0a066cefb7fa9723242540b141242db9db7a1609131ead85313802ea62d35e20 AS builder

RUN apt-get update && apt-get install --yes --no-install-recommends jq && rm -rf /var/lib/apt/lists/*

# Set up the environment
ARG HASKELL_VERSION=9.6.7
ENV STACK_ROOT=/opt/test-runner/.stack
ENV LANG=C.UTF-8
ENV PATH=/root/.cabal/bin:/root/.local/bin:/opt/ghc/${HASKELL_VERSION}/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin

WORKDIR /opt/test-runner/

COPY pre-compiled/ .
RUN stack build --resolver lts-22.44 --no-terminal --test --no-run-tests

COPY ./test-setup/ /opt/test-runner/test-setup/
RUN mkdir /opt/test-runner/bin/ && cd /opt/test-runner/test-setup/ && stack build setup-tests --copy-bins --local-bin-path /opt/test-runner/bin/

COPY . .

# The base image ships GHC 9.10.3, but lts-22.44 needs GHC 9.6.7, so stack
# installed its own copy under STACK_ROOT/programs. That stack-managed GHC is
# the only one the runner uses, and it is the only thing copied into the final
# stage. Before that copy, shrink it: a test runner never builds with profiling
# and never reads the bundled Haddock, so drop the profiling libraries/interfaces
# and the docs. Also drop the leap package's own build artifacts (the snapshot
# under STACK_ROOT is what the runner reuses).
RUN set -eux; \
    rm -rf /opt/test-runner/.stack-work /opt/test-runner/test-setup/.stack-work; \
    find /opt/test-runner/.stack -type f \( -name '*_p.a' -o -name '*.p_hi' \) -delete; \
    find /opt/test-runner/.stack -depth -type d \( -name doc -o -name man -o -name haddock \) -exec rm -rf {} +

# ---- runner: slim base + only what GHC needs to compile & link each solution ----
# Pinned Debian 13 (trixie) slim, matching the lean/racket/ocaml/dart/vlang test
# runners so the base layer is shared (deduplicated) on Exercism's servers.
FROM debian:trixie-slim@sha256:109e2c65005bf160609e4ba6acf7783752f8502ad218e298253428690b9eaa4b AS runner

# stack compiles each solution at runtime, so GHC needs a C toolchain and the
# libraries it links against:
#   gcc, binutils, libc6-dev - the C compiler, assembler/linker and libc headers
#   binutils-gold            - GHC's settings call for ld.gold, which Debian 13
#                              split out of binutils (the bindist was built with it)
#   libgmp-dev               - GHC's integer-gmp backend links -lgmp
#   libffi-dev               - the GHC RTS links -lffi
#   libnuma-dev              - the GHC RTS links -lnuma
#   libtinfo6                - the ghc-tinfo6 build loads libtinfo.so.6
#   jq                       - run.sh assembles results.json with it
RUN apt-get update \
    && apt-get install --yes --no-install-recommends \
        gcc binutils binutils-gold libc6-dev libgmp-dev libffi-dev libnuma-dev libtinfo6 jq \
    && rm -rf /var/lib/apt/lists/*

# Set up the environment
ENV STACK_ROOT=/opt/test-runner/.stack
ENV LANG=C.UTF-8
ENV PATH=/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin

# The stack-managed GHC and dependency snapshot live under /opt/test-runner; the
# bindist records absolute paths, so it must land at the same path it built at.
COPY --from=builder /usr/local/bin/stack /usr/local/bin/stack
COPY --from=builder /opt/test-runner /opt/test-runner

WORKDIR /opt/test-runner

ENTRYPOINT ["/opt/test-runner/bin/run.sh"]
