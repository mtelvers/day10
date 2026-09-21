refresh-base decides whether there is anything to do before it starts a
container, so those decisions can be checked here.

  $ export HOME=$PWD
  $ refresh() {
  >   day10 refresh-base --cache-dir cache \
  >     --os-distribution debian --os-version 13 --arch x86_64 "$@"
  > }

Nothing to refresh when the platform has no base image, and that is not an
error: a builder that has never built for a platform has nothing stale.

  $ mkdir -p cache
  $ refresh
  [WARNING] No base image for debian-13-x86_64, so nothing to refresh
  $ refresh --max-age 24
  [WARNING] No base image for debian-13-x86_64, so nothing to refresh

An index refreshed recently is left alone, so this is safe to call on every
idle window rather than only when something is known to be stale.

  $ mkdir -p cache/debian-13-x86_64/base
  $ touch cache/debian-13-x86_64/base/refreshed
  $ refresh --max-age 24
  [NOTE] Index for debian-13-x86_64 refreshed 0 hour(s) ago, within 24
