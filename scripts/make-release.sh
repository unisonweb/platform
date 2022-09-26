#!/bin/bash

set -e

if [[ "$1" = "--status" ]]; then
    gh workflow view release --repo unisonweb/unison
    gh workflow view release --repo unisonweb/homebrew-unison
fi

usage() {
    echo "NOTE: must be run from the root of the project."
    echo "Usage: $0 VERSION [TARGET]"
    echo "VERSION: The version you're releasing, e.g. M4a"
    echo "TARGET: The revision to make the release from, defaults to 'trunk'"
    echo ""
    echo "E.g."
    echo "$0 M4a"
}

if [[ -z "$1"  ]] ; then
  usage
  exit 1
fi

if ! command -V "gh" >/dev/null 2>&1; then
   echo "Required command \`gh\` not found, find installation instructions here: https://cli.github.com/manual/installation"
   exit 1
fi

if ! [[ "$1" =~ ^M[0-9]+[a-z]?$ ]] ; then
 echo "Version tag must be of the form 'M4' or 'M4a'"
 usage
 exit 1
fi

version="${1}"
prev_version=$(./scripts/previous-tag.sh "$version")
target=${2:-trunk}
tag="release/${version}"

echo "Creating release in unison-local-ui..."
gh release create "release/${version}" --repo unisonweb/unison-local-ui --target main --generate-notes --notes-start-tag "release/${prev_version}"

echo "Kicking off release workflow in unisonweb/unison"
git tag "${tag}" "${target}"
git push origin "${tag}"
gh workflow run release --repo unisonweb/unison --field "version=${version}"

echo "Kicking off Homebrew update task"
gh workflow run release --repo unisonweb/homebrew-unison --field "version=${version}"

echo "Opening relevant workflows in browser"
gh workflow view release --web --repo unisonweb/homebrew-unison || true
gh workflow view release --web --repo unisonweb/unison || true

echo "Okay! All the work has been kicked off, it may take several hours to complete."
echo "Run '$0 --status' to see job status."
