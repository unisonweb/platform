#!/usr/bin/env sh
set -ex

# the first arg is the path to the unison executable
if [ -z "$1" ]; then
  echo "Usage: $0 <path-to-ucm>"
  exit 1
fi

# call unison with all its args quoted
"$@" transcript unison-src/tests/fix5507.md \
  && "$@" run.compiled fix5507.uc
