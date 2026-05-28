#!/usr/bin/env bash

# Synopsis:
# Run the test runner on a solution.

# Arguments:
# $1: exercise slug
# $2: absolute path to solution folder
# $3: absolute path to output directory

# Output:
# Writes the test results to a results.json file in the passed-in output directory.
# The test results are formatted according to the specifications at https://github.com/exercism/docs/blob/main/building/tooling/test-runners/interface.md

# Example:
# ./bin/run.sh two-fer /absolute/path/to/two-fer/solution/folder/ /absolute/path/to/output/directory/

resolver=lts-22.44

set -euo pipefail

# If any required arguments is missing, print the usage and exit
if [ -z "$1" ] || [ -z "$2" ] || [ -z "$3" ]; then
    echo "usage: ./bin/run.sh exercise-slug /absolute/path/to/two-fer/solution/folder/ /absolute/path/to/output/directory/"
    exit 1
fi

slug="$1"
input_dir="${2%/}"
output_dir="${3%/}"
setup_tests_executable="bin/setup-tests"
tmp=$(mktemp -d)
trap 'rm -rf "${tmp}"' EXIT

# Back up the input directory to restore later.
cp -Rp "${input_dir}" "${tmp}"

# Create the output directory if it doesn't exist
mkdir -p "${output_dir}"

echo "${slug}: testing..."

{
    echo "resolver: ${resolver}"
    echo "system-ghc: true"
} > "${input_dir}/stack.yaml"

# Run our test setup which does some code injection to modify how the tests
# will run to use our custom hspec formatter that outputs results.json automatically.
# We expect the setup-tests executable to be pre-built in Docker, but fallback to using runghc in case it isn't
# found so that developers can continue to easily run `bin/run.sh` locally.
if [ -x "${setup_tests_executable}" ]; then
    "${setup_tests_executable}" "${input_dir}"
else
    echo "Did not find bin/setup-tests executable - using stack runghc ./test-setup/src/Main.hs instead"
    stack --resolver "${resolver}" runghc ./test-setup/src/Main.hs "${input_dir}"
fi

cd "${input_dir}"

# Run the tests for the provided implementation file and redirect stdout and
# stderr to capture it.
# Use cmd || true to avoid exiting if tests fail (set -e).
test_output=$(stack build --resolver "${resolver}" --test --allow-different-user 2>&1) || true

# Copy results.json to the output directory (only if output directory is different from
# the input directory).
# This file may not exist if the test failed to run, eg on a compiler error.
if [ "${output_dir}" != "${input_dir}" ] && [ -e "${input_dir}/results.json" ]; then
    mv "${input_dir}/results.json" "${output_dir}/results.json"
fi

# If the results.json file does not exist, it means that the tests failed to run
# (usually this would be a compiler error)
results_file="${output_dir}/results.json"
if ! [ -f "${results_file}" ]; then
    # Sanitize the output
    if grep -q "Registering library for " <<< "${test_output}" ; then
        test_output=$(sed -n -E -e '1,/^Registering library for/!p' <<< "${test_output}")
    fi
    if grep -q "Building library for " <<< "${test_output}" ; then
        test_output=$(sed -n -E -e '1,/^Building library for/!p' <<< "${test_output}")
    fi

    jq -n --arg output "${test_output}" '{version: 2, status: "error", message: $output}' > "${results_file}"
fi

# Restore state
for file in 'stack.yaml' 'package.yaml' 'test/Tests.hs'; do
    cat "${tmp}/${input_dir##*/}/${file}" > "${input_dir}/${file}"
done
for file in 'test/HspecFormatter.hs' '.stack-work' "${slug}.cabal"; do
    rm -rf "${input_dir}/${file}"
done
# Drop the tmp dir
rm -rf "${tmp}"

echo "${slug}: done"
