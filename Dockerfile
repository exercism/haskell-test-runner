FROM haskell:9.2.7-buster

RUN apt-get update && \
    apt-get install -y jq && \
    apt-get purge --auto-remove && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/*

ENV STACK_ROOT=/opt/test-runner/.stack

WORKDIR /opt/test-runner/

COPY pre-compiled/ .
RUN stack build --resolver lts-20.11 --no-terminal --test --no-run-tests

COPY . .
ENTRYPOINT ["/opt/test-runner/bin/run.sh"]
