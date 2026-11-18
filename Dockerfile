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

# bin/fast-run.sh calls this GHC directly against this snapshot database. It
# also needs the unit id of every package in the global and snapshot
# databases, listed one per line as "name unit-id".
RUN set -eux; \
    ghc=$(stack path --resolver lts-22.44 --compiler-exe); \
    ghc_pkg=$(dirname "${ghc}")/ghc-pkg; \
    snapshot_db=$(stack path --resolver lts-22.44 --snapshot-pkg-db); \
    printf 'GHC=%s\nGHC_PKG=%s\nSNAPSHOT_DB=%s\n' "${ghc}" "${ghc_pkg}" "${snapshot_db}" > /opt/test-runner/fast-run.env; \
    mkdir -p /opt/test-runner/fast-run; \
    "${ghc_pkg}" --global --package-db "${snapshot_db}" list --simple-output --show-unit-ids \
        | tr ' ' '\n' | sed -E 's/^(.*)-([0-9][0-9.]*)(-[A-Za-z0-9]+)?$/\1 &/' | sort > /opt/test-runner/fast-run/units.txt; \
    test -z "$(cut -d' ' -f1 /opt/test-runner/fast-run/units.txt | uniq -d)"

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
FROM debian:trixie-slim@sha256:a29215f6a35e51e22adffa17f89e9d2ef06214e64a2bad10d765c46aea49f11f AS runner

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
#   mold                     - bin/fast-run.sh links the tests with it, in
#                              about a third of the time ld.gold takes
RUN apt-get update \
    && apt-get install --yes --no-install-recommends \
        gcc binutils binutils-gold libc6-dev libgmp-dev libffi-dev libnuma-dev libtinfo6 jq mold \
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

# Run the fast path once on the pre-compiled leap project, and keep the
# compiled HspecFormatter, so that no run has to compile it again. The build
# fails if the fast path doesn't work.
RUN set -eux; \
    mkdir /tmp/precompile; \
    cp -R pre-compiled /tmp/precompile/leap; \
    FAST_RUN_SAVE_FORMATTER=1 bin/run.sh leap /tmp/precompile/leap /tmp/precompile/out; \
    jq -e '.tests | length > 0' /tmp/precompile/out/results.json; \
    test -f /opt/test-runner/fast-run/HspecFormatter.hi; \
    rm -rf /tmp/precompile

ENTRYPOINT ["/opt/test-runner/bin/run.sh"]
