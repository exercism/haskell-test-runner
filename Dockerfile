ARG VERSION=9.2.7

FROM haskell:${VERSION}-slim-buster

RUN apt-get update && \
    apt-get install -y jq && \
    apt-get purge --auto-remove && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/*

ENV STACK_ROOT=/opt/test-runner/.stack

WORKDIR /opt/test-runner/

COPY pre-compiled/ .
RUN stack build --resolver lts-20.18 --no-terminal --test --no-run-tests && \
    rm -rf /opt/ghc \
    /opt/test-runner/.stack/programs/x86_64-linux/ghc-tinfo6-${VERSION}/share \
    /opt/test-runner/.stack/programs/x86_64-linux/ghc-tinfo6-${VERSION}.tar.xz

COPY . .
ENTRYPOINT ["/opt/test-runner/bin/run.sh"]
