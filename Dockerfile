ARG HASKELL_VERSION=9.6.7
FROM haskell:${HASKELL_VERSION}-slim-bookworm@sha256:0a066cefb7fa9723242540b141242db9db7a1609131ead85313802ea62d35e20 AS build

RUN apt-get update \
    && apt-get install --yes --no-install-recommends jq \
    && rm -rf /var/lib/apt/lists/*

# Set up the environment
ARG HASKELL_VERSION=9.6.7
ENV STACK_ROOT=/opt/test-runner/.stack
ENV LANG=C.UTF-8
ENV PATH=/usr/local/bin:/usr/bin

# Precompile all the needed libraries.
COPY pre-compiled/ /opt/test-runner/
RUN cd /opt/test-runner/ && stack build --resolver lts-22.44 --no-terminal --test --no-run-tests

# Precompile the test setup tool.
COPY ./test-setup/ /opt/test-runner/test-setup/
RUN cd /opt/test-runner/test-setup/ && stack build setup-tests --copy-bins --local-bin-path /opt/test-runner/bin/

# Copy the test runner scripts and helpers.
COPY . /opt/test-runner/

# Clean up unneeded packages and files.
RUN apt-get purge --yes ca-certificates curl dpkg-dev g++ git gnupg libdpkg-perl make netbase xz-utils \
    && apt-get autoremove --yes \
    && apt-get clean \
    && find / \( -name doc -o -name man \) -exec rm -rf {} + \
    && rm -rf /opt/ghc/9.10.3/

# Flatten the image, clearing out deleted files from prior layers.
FROM scratch
COPY --from=build / /

# Set up the environment
ENV STACK_ROOT=/opt/test-runner/.stack
ENV LANG=C.UTF-8
ENV PATH=/usr/local/bin:/usr/bin

WORKDIR /opt/test-runner

ENTRYPOINT ["/opt/test-runner/bin/run.sh"]
