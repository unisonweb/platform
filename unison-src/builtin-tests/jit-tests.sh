#!/usr/bin/env sh
set -ex

# the first arg is the path to the unison executable
if [ -z "$1" ]; then
  echo "Usage: $0 <path/flags for calling unison w/ jit>"
  echo "Example: $0 ./unison --runtime-path ./runtime/bin/unison-runtime"
  exit 1
fi

runtime_tests_version="@unison/runtime-tests/releases/0.0.1"
echo $runtime_tests_version

codebase=${XDG_CACHE_HOME:-"$HOME/.cache"}/unisonlanguage/runtime-tests.unison

dir=${XDG_DATA_HOME:-"$HOME/.local/share"}/unisonlanguage/scheme-libs
echo $dir

mkdir -p $dir
cp -r scheme-libs/* $dir/

runtime_tests_version="$runtime_tests_version" \
    envsubst '$runtime_tests_version' \
    < unison-src/builtin-tests/jit-tests.tpl.md \
    > unison-src/builtin-tests/jit-tests.md

# call unison with all its args quoted
time "$@" transcript.fork -C $codebase -S $codebase unison-src/builtin-tests/jit-tests.md
