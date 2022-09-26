# Release Steps

## 1. (Major milestones only) New Base Release

Cut a release of base. @runarorama does this usually.


```
.> pull git(git@github.com:unisonweb/base) basedev.release
.> cd .basedev.release
.basedev.release> delete.namespace releases._latest
.basedev.release> squash trunk releases._<ReleaseName>
```

Edit `releases._<ReleaseName>.README` to include `Release: <ReleaseName>`.

```
.basedev.release> fork releases._<ReleaseName> releases._latest
.basedev.release> push git(git@github.com:unisonweb/base)
```

## 2. Run Release script

* **Milestone Release**: Look up the most recent release; bump the number and remove any trailing letters, e.g. `./scripts/make-release release/M5 trunk`
* **Minor Release**: Increment the trailing letter of the previous release, or add an `a` to the previous milestone release, e.g. `./scripts/make-release release/M5a trunk`

Then, using the new release version, from the root of the `unisonweb/unison` project run:

```sh
./scripts/make_release.sh <VERSION> [TARGET (defaults to trunk)]
```

This will tag the appropriate versions in all the required projects, and kick off all of the necessary CI jobs to ship a release.

Including:

* A release workflow in `unisonweb/unison` to build UCM on multiple platforms, create a release with appropriate release notes from the previous release, and upload the artifacts to that release.
* A release workflow in `unison-local-ui` to build UCM on multiple platforms, create a release with appropriate release notes from the previous release, and upload the artifacts to that release.
* A release workflow in `homebrew-unison` to wait for artifacts to be uploaded, then download those artifacts, get the checksums, and create an up-to-date homebrew formula.

After successfully executing the script you just have to sit tight and wait for all the jobs to complete.

## 3

Smoke test of the new release. Try `brew upgrade unison-language`, launch it, launch `ui`.

## 4

Announce on #general Slack channel. Template below.

---

Release announcement template (be sure to update the release urls) -

We've just released a new version of Unison, $RELEASE_NAME, release notes here (link to the issue). Install/upgrade instructions in the thread.

Mac upgrade is just `brew upgrade unison-language`.

A fresh install via:

```
brew tap unisonweb/unison
brew install unison-language
```

If you have previously done brew install unison-language --head to install a dev build, uninstall that first via brew uninstall unison-language.

_Linux manual install:_

```
mkdir unisonlanguage
curl -L https://github.com/unisonweb/unison/releases/download/release%2FM2h/ucm-linux.tar.gz --output unisonlanguage/ucm.tar.gz
tar -xzf unisonlanguage/ucm.tar.gz -C unisonlanguage
./unisonlanguage/ucm
```

_Mac manual install:_

```
mkdir unisonlanguage
curl -L https://github.com/unisonweb/unison/releases/download/release%2FM2h/ucm-macos.tar.gz --output unisonlanguage/ucm.tar.gz
tar -xzf unisonlanguage/ucm.tar.gz -C unisonlanguage
./unisonlanguage/ucm
```
