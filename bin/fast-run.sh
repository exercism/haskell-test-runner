#!/usr/bin/env bash

# Synopsis:
# Build and run a solution's tests by calling GHC directly, without stack.
#
# `stack build --test` spends most of a run on work that is the same for every
# solution: starting stack, configuring the package with Cabal, compiling our
# HspecFormatter module, building a shared library, and installing and
# registering the package. This script compiles the library and the test suite
# with the flags Cabal uses, against the same prebuilt snapshot. It gives the
# library the same unit id as Cabal would, because GHC prints the unit id in
# call stacks (for example when a solution calls `error`). It links the tests
# with mold, which is much faster than ld.gold. HspecFormatter is compiled once
# at image build.
#
# It only handles the package.yaml shapes used by the track. If it sees
# anything else, or if anything fails (such as a compile error), it exits with
# a non-zero status and run.sh runs the old `stack build` path, so error
# messages stay exactly as they were.

# Arguments:
# $1: absolute path to the solution folder (already modified by setup-tests)
# $2: package.yaml as JSON, written by setup-tests

# Output:
# Writes results.json to the solution folder, and exits 0 if it did.

set -euo pipefail
export LC_ALL=C.UTF-8

solution="$1"
package_json="$2"

# fast-run.env is written at image build. It sets GHC and GHC_PKG (the
# stack-managed compiler) and SNAPSHOT_DB (the prebuilt snapshot's package
# database).
# shellcheck source=/dev/null
source /opt/test-runner/fast-run.env
precompiled=/opt/test-runner/fast-run

# Print a field that hpack allows as either a string or a list, one per line.
list() { jq -r "($1 // []) | if type == \"string\" then [.] else . end | .[]" "${package_json}"; }

# Only accept keys whose meaning we reproduce. The fields that only describe
# the package (author, license and so on) don't change the build. stack
# doesn't build benchmarks for `--test`.
allowed_top='["name","version","dependencies","library","tests","benchmarks","ghc-options","default-extensions","synopsis","description","category","author","maintainer","copyright","license","license-file","github","homepage","bug-reports","extra-source-files","stability"]'
allowed_component='["main","source-dirs","dependencies","ghc-options","default-extensions","exposed-modules","other-modules"]'
jq -e --argjson top "${allowed_top}" --argjson comp "${allowed_component}" '
    (keys - $top == [])
    and (.name | type == "string")
    and ((.version // "0.0.0") | type == "string" and test("^[0-9]+(\\.[0-9]+)*$"))
    and (.library | type == "object" and (keys - $comp == []) and has("exposed-modules") and has("source-dirs"))
    and (.tests | type == "object" and length == 1)
    and (.tests[] | type == "object" and (keys - $comp == []) and has("main") and has("source-dirs"))
' "${package_json}" > /dev/null

name=$(jq -r '.name' "${package_json}")
version=$(jq -r '.version // "0.0.0"' "${package_json}")
test_suite=$(jq -r '.tests | keys[0]' "${package_json}")
lib='.library'
tst=".tests[\"${test_suite}\"]"

# Dependencies must be plain package names from the image's package databases.
# Version ranges and anything else go to stack. A package named like one in
# the snapshot is fine here, because GHC doesn't need the snapshot to change.
deps() {
    local dep
    for dep in $(list '.dependencies') $(list "$1.dependencies"); do
        [[ "${dep}" =~ ^[A-Za-z0-9-]+$ ]] || return 1
        [[ "${dep}" == "${name}" ]] || grep -q "^${dep} " "${precompiled}/units.txt" || return 1
        echo "${dep}"
    done | sort -u
}
lib_deps=$(deps "${lib}")
test_deps=$(deps "${tst}")
! grep -qx -- "${name}" <<< "${lib_deps}"

# Look up the unit ids of package names (other than this package itself).
unit_ids() {
    local dep
    for dep in "$@"; do
        [[ "${dep}" == "${name}" ]] && continue
        grep "^${dep} " "${precompiled}/units.txt" | cut -d' ' -f2
    done
}

# Accept only warning and runtime options, so that every module is compiled
# with the same optimisation and language settings as under Cabal.
ghc_options() {
    local opt
    for opt in $(list '.["ghc-options"]') $(list "$1[\"ghc-options\"]"); do
        [[ "${opt}" =~ ^-W && "${opt}" != *Werror* ]] || [[ "${opt}" =~ ^(-threaded|-rtsopts|-with-rtsopts=.*)$ ]] || return 1
        echo "${opt}"
    done
}
lib_options=$(ghc_options "${lib}")
test_options=$(ghc_options "${tst}")

extensions() {
    local ext
    for ext in $(list '.["default-extensions"]') $(list "$1[\"default-extensions\"]"); do
        # CPP would need the version macros Cabal generates.
        [[ "${ext}" =~ ^[A-Za-z0-9]+$ && "${ext}" != CPP ]] || return 1
        echo "-X${ext}"
    done
}
lib_extensions=$(extensions "${lib}")
test_extensions=$(extensions "${tst}")

cd "${solution}"
rm -f results.json

# hpack lists every module in a component's source folders, so Cabal compiles
# them all, even ones nothing imports. Print those modules. Files that need a
# preprocessor, CPP or the generated Paths_ module go to stack.
modules_in() {
    local dir
    for dir in "$@"; do
        [[ -d "${dir}" ]] || return 1
        ! find "${dir}" -type f \( -name '*.lhs' -o -name '*.hsc' -o -name '*.x' -o -name '*.y' -o -name '*.hs-boot' \) | grep -q . || return 1
        ! find "${dir}" -type f -name '*.hs' -exec grep -lqE 'LANGUAGE[^#]*CPP|-XCPP|import +(qualified +)?Paths_' {} + || return 1
        (cd "${dir}" && find . -type f -name '*.hs' | sed 's|^\./||; s|\.hs$||; s|/|.|g' | sort)
    done
}
mapfile -t lib_dirs < <(list "${lib}[\"source-dirs\"]")
mapfile -t test_dirs < <(list "${tst}[\"source-dirs\"]")
found_lib_modules=$(modules_in "${lib_dirs[@]}")
found_test_modules=$(modules_in "${test_dirs[@]}")

# Cabal passes the exposed and other modules as targets. If other-modules
# isn't given, hpack fills it in with the rest of the source folders.
mapfile -t exposed_modules < <(list "${lib}[\"exposed-modules\"]")
if jq -e "${lib} | has(\"other-modules\")" "${package_json}" > /dev/null; then
    mapfile -t other_modules < <(list "${lib}[\"other-modules\"]")
else
    mapfile -t other_modules < <(grep -vxF -f <(printf '%s\n' "${exposed_modules[@]}") <<< "${found_lib_modules}" || true)
fi

main_file=$(jq -r "${tst}.main" "${package_json}")
main_path=""
for dir in "${test_dirs[@]}"; do
    if [[ -f "${dir}/${main_file}" ]]; then main_path="${dir}/${main_file}"; break; fi
done
[[ -n "${main_path}" ]]
main_module=$(sed 's|\.hs$||; s|/|.|g' <<< "${main_file}")
if jq -e "${tst} | has(\"other-modules\")" "${package_json}" > /dev/null; then
    mapfile -t test_modules < <(list "${tst}[\"other-modules\"]")
else
    mapfile -t test_modules < <(grep -vx -- "${main_module}" <<< "${found_test_modules}" || true)
fi

# Cabal's unit id hashes the package id with the unit ids of all the package's
# dependencies (library and tests together), sorted by name.
package_id="${name}-${version}"
unit_id=$(/opt/test-runner/bin/cabal-unit-id "${package_id}" $(unit_ids $(sort -u <<< "${lib_deps}"$'\n'"${test_deps}")))

# GHC's check for whether HspecFormatter needs recompiling includes the
# output folders, so they must be the same at image build and at run time.
# Each container runs one solution, so a fixed path is safe.
build=/tmp/fast-run-build
rm -rf "${build}"
trap 'rm -rf "${build}"' EXIT
mkdir -p "${build}/lib" "${build}/test" "${build}/db"

# These are the flags Cabal passes to GHC, minus the ones for the shared
# library, the Paths_ module and its generated include folders.
common=(--make -fbuilding-cabal-package -O -static
        -hide-all-packages -no-user-package-db -package-db "${SNAPSHOT_DB}")
output_dirs() { echo "-outputdir $1 -odir $1 -hidir $1 -stubdir $1"; }

# Compile the library as its own unit, with only its own dependencies visible.
lib_includes=(-i)
for dir in "${lib_dirs[@]}"; do lib_includes+=("-i${dir}"); done
"${GHC}" "${common[@]}" -no-link $(output_dirs "${build}/lib") "${lib_includes[@]}" \
    -this-unit-id "${unit_id}" \
    $(for d in $(unit_ids ${lib_deps}); do echo "-package-id ${d}"; done) \
    -XHaskell2010 ${lib_extensions} "${exposed_modules[@]}" "${other_modules[@]}" ${lib_options} \
    > /dev/null 2>&1

# Register the library in a package database of its own, as Cabal does, so the
# tests can only import its exposed modules.
find "${build}/lib" -name '*.o' -print0 | xargs -0 ar rcs "${build}/lib/libHS${unit_id}.a"
{
    echo "name: ${name}"
    echo "version: ${version}"
    echo "id: ${unit_id}"
    echo "key: ${unit_id}"
    echo "exposed: True"
    echo "exposed-modules: ${exposed_modules[*]}"
    echo "hidden-modules: ${other_modules[*]}"
    echo "import-dirs: ${build}/lib"
    echo "library-dirs: ${build}/lib"
    echo "hs-libraries: HS${unit_id}"
    echo "depends: $(unit_ids ${lib_deps} | tr '\n' ' ')"
} > "${build}/db/${unit_id}.conf"
"${GHC_PKG}" --package-db "${build}/db" recache > /dev/null 2>&1

# Our HspecFormatter only depends on the snapshot, so it was compiled at image
# build with these same flags. GHC sees the matching interface and skips it.
if [[ -z "${FAST_RUN_SAVE_FORMATTER:-}" ]]; then
    cp "${precompiled}"/HspecFormatter.{hi,o} "${build}/test/"
fi

# Compile and link the tests, with mold as the linker.
test_includes=(-i)
for dir in "${test_dirs[@]}"; do test_includes+=("-i${dir}"); done
"${GHC}" "${common[@]}" -package-db "${build}/db" $(output_dirs "${build}/test") "${test_includes[@]}" \
    $(for d in ${test_deps}; do
        if [[ "${d}" == "${name}" ]]; then echo "-package-id ${unit_id}"; else echo "-package-id $(unit_ids "${d}")"; fi
      done) \
    -XHaskell2010 ${test_extensions} "${main_path}" "${test_modules[@]}" ${test_options} \
    -optl-fuse-ld=mold -o "${build}/test/test" > /dev/null 2>&1

# At image build, keep the compiled HspecFormatter for every later run.
if [[ -n "${FAST_RUN_SAVE_FORMATTER:-}" ]]; then
    cp "${build}"/test/HspecFormatter.{hi,o} "${precompiled}/"
fi

# stack runs the tests from the package folder, where HspecFormatter writes
# results.json.
"${build}/test/test" > /dev/null 2>&1 || true
[[ -f "${solution}/results.json" ]]
