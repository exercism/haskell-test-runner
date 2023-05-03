FROM ubuntu:22.10

RUN apt-get update && \
    apt-get install -y build-essential curl git jq libffi-dev libgmp-dev libgmp10 libncurses-dev libncurses5 libtinfo5 zlib1g-dev && \
    apt-get purge --auto-remove && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/*

RUN curl -sSL https://get.haskellstack.org/ | sh

ENV STACK_ROOT=/opt/test-runner/.stack

WORKDIR /opt/test-runner/

COPY pre-compiled/ .
RUN stack build --resolver lts-20.18 --no-terminal --test --no-run-tests

COPY . .
ENTRYPOINT ["/opt/test-runner/bin/run.sh"]
