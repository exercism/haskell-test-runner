FROM haskell:9.0.1-buster

# RUN stack install hspec

# ENV STACK_ROOT=/tmp/.stack

WORKDIR /opt/test-runner

# COPY stack.yaml .
# RUN stack setup --verbose

COPY . .
ENTRYPOINT ["/opt/test-runner/bin/run.sh"]
