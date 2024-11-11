#!/bin/bash
set -ex

if [ -z "$1" ]; then
  ucm=$(stack exec -- which unison)
else
  ucm="$1"
fi

runtime_tests_version="@unison/runtime-tests/releases/0.0.1"
echo $runtime_tests_version

codebase=${XDG_CACHE_HOME:-"$HOME/.cache"}/unisonlanguage/runtime-tests.unison

runtime_tests_version="$runtime_tests_version" \
    envsubst '$runtime_tests_version' \
    < unison-src/builtin-tests/interpreter-tests.tpl.md \
    > unison-src/builtin-tests/interpreter-tests.md
echo "$ucm" transcript.fork -C $codebase -S $codebase unison-src/builtin-tests/interpreter-tests.md
time "$ucm" transcript.fork -C $codebase -S $codebase unison-src/builtin-tests/interpreter-tests.md
