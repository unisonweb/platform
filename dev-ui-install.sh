#!/usr/bin/env sh
set -eu

echo "This script downloads the latest Unison Local UI release"
echo "and puts it in the correct spot next to the unison"
echo "executable built by stack."
echo ""

stack build
curl -L https://github.com/unisonweb/unison-local-ui/releases/download/latest/unisonLocal.zip --output unisonLocal.zip
parent_dir="$(dirname -- $(stack exec which unison))"
mkdir -p "$parent_dir/ui"
unzip -q -o unisonLocal.zip -d "$parent_dir/ui"
