#!/bin/bash

set -e

if [[ "$1" = "--status" ]]; then
    gh workflow view release --repo unisonweb/unison
    gh workflow view release --repo unisonweb/homebrew-unison
fi

prev_tag="$(gh release view --json tagName -t '{{printf .tagName}}')"

usage() {
    prev_version="${prev_tag#release/}"
    prefix="${prev_version%.*}"
    next_version="${prefix}.$(( ${prev_version##*.} + 1 ))"
    echo "usage: $0 <version> [ref]"
    echo ""
    echo "version: The new version number"
    echo "ref: The Git revision to make the release from, defaults to 'origin/trunk'"
    echo ""
    echo "Try: $0 $next_version"
}

if [[ -z "$1" ]] ; then
  usage
  exit 1
fi

if ! command -V gh >/dev/null 2>&1; then
   echo "Required command \`gh\` not found, find installation instructions here: https://cli.github.com/manual/installation"
   exit 1
fi

if ! [[ "$1" =~ ^[0-9]+\.[0-9]+\.[0-9]+$ ]] ; then
 echo "Version tag must be of the form 'x.y.z' where x, y, and z are nonnegative integers."
 usage
 exit 1
fi

version="${1}"
src=${2:-origin/trunk}
tag="release/$version"

echo "Creating release in unison-local-ui."
gh release create "release/${version}" \
  --repo unisonweb/unison-local-ui \
  --target main \
  --generate-notes --notes-start-tag "$prev_tag"

echo "Kicking off release workflow in unisonweb/unison"
# Make sure our origin/trunk ref is up to date, since that's usually what gets tagged.
git fetch origin trunk
git tag "${tag}" "${src}"
git push origin "${tag}"
gh workflow run release.yaml --repo unisonweb/unison \
  --ref "${tag}" \
  --field "version=${version}"

echo "Kicking off Homebrew update task"
gh workflow run release.yaml --repo unisonweb/homebrew-unison --field "version=${version}"

echo "Opening relevant workflows in browser"
gh workflow view release.yaml --web --repo unisonweb/homebrew-unison || true
gh workflow view release.yaml --web --repo unisonweb/unison || true

echo "Okay! All the work has been kicked off, it may take several hours to complete."
echo "Run '$0 --status' to see job status."
