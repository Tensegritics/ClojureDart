# Releasing ClojureDart

Releases are versioned GitHub Releases named `0.9.YYYYMMDD` using the current
UTC date. Additional releases on the same UTC day use the suffixes `a` through
`z`, for example `0.9.20260822a`. Release tags must never be moved and
published assets must never be replaced. Each release contains a
`deps.latest.edn` asset pinned to both its tag and full Git SHA.

## Prerequisites

- Run from the root of the ClojureDart repository on `main`.
- Commit all tracked changes.
- Push `main` to the `release` remote.
- Install [Babashka](https://babashka.org/).
- Install and authenticate [GitHub CLI](https://cli.github.com/).

Authenticate GitHub CLI with:

```shell
gh auth login -h github.com
gh auth status -h github.com
```

The release script explains every failed prerequisite and prints the commands
needed to correct and verify it.

## Check

Run all non-mutating checks:

```shell
./release --check
```

The script requires tracked files to be clean. Untracked files and a failing or
missing CI run produce warnings but do not block a release.

## Test

Run the Babashka release tests and JVM upgrader tests:

```shell
./release --test
```

## Publish

```shell
./release
```

The script calculates the current `YYYYMMDD` date in UTC. The first release of
the day has no suffix; subsequent releases use `a`, `b`, and so on through
`z`. A release interrupted after creating its tag resumes that same tag even
after the UTC date changes. Draft releases are completed on retry, and an
already-published release is verified again instead of duplicated. It then:

1. generates `deps.latest.edn` in a temporary directory;
2. creates the tag and GitHub Release for the exact SHA already pushed to
   `release/main`;
3. uploads the asset and marks the release as latest;
4. verifies the remote tag and downloads the asset for a byte-for-byte check.

The script never pushes `main`, moves a tag, or automatically deletes a partial
release. If GitHub fails partway through, inspect the state with the commands
printed by the error message before retrying.

## Legacy upgrade bridge

Old ClojureDart versions discover releases through `.hashes`. During migration,
its last entry must be the SHA of the final bridge commit containing the
release-based upgrader. Keep that file afterward so old clients can reach the
bridge version, but do not append ordinary release SHAs.

Users upgrading from a version older than the bridge may exceptionally need to
run the command twice:

```shell
clj -M:cljd upgrade
clj -M:cljd upgrade
```

The first invocation reaches the bridge through `.hashes`; the second uses the
latest GitHub Release asset.
