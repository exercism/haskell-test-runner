#!/usr/bin/env bash

rm -f ./tests/example-all-fail/test/HspecFormatter.hs \
  ./tests/example-empty-file/test/HspecFormatter.hs \
  ./tests/example-partial-fail/test/HspecFormatter.hs \
  ./tests/example-success/test/HspecFormatter.hs \
  ./tests/example-syntax-error/test/HspecFormatter.hs
  ./tests/example-all-fail/results.json \
  ./tests/example-empty-file/results.json \
  ./tests/example-partial-fail/results.json \
  ./tests/example-success/results.json \
  ./tests/example-syntax-error/results.json

git checkout tests/example-all-fail/package.yaml tests/example-all-fail/test/Tests.hs \
  tests/example-empty-file/package.yaml tests/example-empty-file/test/Tests.hs \
  tests/example-partial-fail/package.yaml tests/example-partial-fail/test/Tests.hs \
  tests/example-success/package.yaml tests/example-success/test/Tests.hs \
  tests/example-syntax-error/package.yaml tests/example-syntax-error/test/Tests.hs

