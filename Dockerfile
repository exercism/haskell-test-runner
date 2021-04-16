FROM haskell:9.0.1-buster

COPY stack.yaml package.yaml ./
RUN stack setup

WORKDIR /opt/test-runner
COPY . .
ENTRYPOINT ["/opt/test-runner/bin/run.sh"]
