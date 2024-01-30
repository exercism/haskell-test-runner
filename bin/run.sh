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

set -euo pipefail

# If any required arguments is missing, print the usage and exit
if [ -z "$1" ] || [ -z "$2" ] || [ -z "$3" ]; then
    echo "usage: ./bin/run.sh exercise-slug /absolute/path/to/two-fer/solution/folder/ /absolute/path/to/output/directory/"
    exit 1
fi

slug="$1"
input_dir="${2%/}"
output_dir="${3%/}"
results_file="${output_dir}/results.json"

# Create the output directory if it doesn't exist
mkdir -p "${output_dir}"

echo "${slug}: testing..."

file_contents=$(< "${input_dir}/stack.yaml")

echo "system-ghc: true" >> "${input_dir}/stack.yaml"

# Run our SetupTestFile which does some code injection to modify how the tests
# will run to use our custom hspec formatter that outputs results.json automatically
# TODO: see if we can avoid runghc and instead run a precompiled binary
stack --resolver lts-20.18 runghc src/SetupTestFile.hs $input_dir

pushd "${input_dir}" > /dev/null

# disable -e since we expect some tests to fail
old_opts=$-
set +e

# Run the tests for the provided implementation file and redirect stdout and
# stderr to capture it
test_output=$(stack build --resolver lts-20.18 --test --allow-different-user 2>&1)
exit_code=$?

# re-enable original options
set -$old_opts

# Remove the .stack-work directory, as its 40MB+ size fills up the test runner's disk
rm -rf .stack-work 

popd

# If the results.json file does not exist, it means that the tests failed to run
# (usually this would be a compiler error)
if ! [ -f ${results_file} ]; then
    # Sanitize the output
    if grep -q "Registering library for " <<< "${test_output}" ; then
	sanitized_test_output=$(printf "${test_output}" | sed -n -E -e '1,/^Registering library for/!p')
    elif grep -q "Building library for " <<< "${test_output}" ; then
	sanitized_test_output=$(printf "${test_output}" | sed -n -E -e '1,/^Building library for/!p')
    else
	sanitized_test_output="${test_output}"
    fi

    # Manually add colors to the output to help scanning the output for errors
    colorized_test_output=$(echo "${sanitized_test_output}" \
	| GREP_COLOR='01;31' grep --color=always -E -e '.*FAILED \[[0-9]+\]$|$')

    jq -n --arg output "${colorized_test_output}" '{version: 2, status: "error", message: $output}' > ${results_file}
fi

echo "$file_contents" > "${input_dir}/stack.yaml"

echo "${slug}: done"
