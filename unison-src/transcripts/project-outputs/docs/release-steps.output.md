# Release Steps

## 1\. (Major milestones only) New Base Release

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

## 2\. Run Release script

  - **Milestone Release**: Look up the most recent release; bump the number and remove any trailing letters, e.g. `./scripts/make-release release/M5 trunk`
  - **Minor Release**: Increment the trailing letter of the previous release, or add an `a` to the previous milestone release, e.g. `./scripts/make-release release/M5a trunk`

Then, using the new release version, from the root of the `unisonweb/unison` project run:

``` sh
./scripts/make_release.sh <VERSION> [TARGET (defaults to trunk)]
```

This will tag the appropriate versions in all the required projects, and kick off all of the necessary CI jobs to ship a release.

Including:

  - A release workflow in `unisonweb/unison` to build UCM on multiple platforms, create a release with appropriate release notes from the previous release, and upload the artifacts to that release.
  - A release workflow in `unison-local-ui` to build UCM on multiple platforms, create a release with appropriate release notes from the previous release, and upload the artifacts to that release.
  - A release workflow in `homebrew-unison` to wait for artifacts to be uploaded, then download those artifacts, get the checksums, and create an up-to-date homebrew formula.

After successfully executing the script you just have to sit tight and wait for all the jobs to complete.

## 3

Smoke test of the new release. Try `brew upgrade unison-language`, launch it, launch `ui`.

## 4

Write up release notes, template below.

Preview the markdown in Slack \#general and tag @paul.

## 5

If there are new builtins, redeploy Share.

## 6

Announce on \#general Discord channel.

-----

@everyone We've just released a new version of Unison, $RELEASE\_NAME.

-----

**macOS or Linux w/ Homebrew:**
Install or upgrade is just `brew upgrade unisonweb/unison/unison-language`.

**macOS or Linux manual install:**
macOS

``` 
mkdir -p unisonlanguage && cd unisonlanguage
curl -L https://github.com/unisonweb/unison/releases/latest/download/ucm-macos.tar.gz \
  | tar -xz
./ucm
```

Linux

``` 
mkdir -p unisonlanguage && cd unisonlanguage
curl -L https://github.com/unisonweb/unison/releases/latest/download/ucm-linux.tar.gz \
    | tar -xz
./ucm
```

**Windows manual install:**

  - Recommended: [Set your default Terminal application](https://devblogs.microsoft.com/commandline/windows-terminal-as-your-default-command-line-experience/) to “Windows Terminal”.
  - Download [the release](https://github.com/unisonweb/unison/releases/latest/download/ucm-windows.zip) and extract it to a location of your choosing.
  - Run `ucm.exe`
