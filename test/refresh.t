refresh-base decides whether there is anything to do before it starts a
container, so those decisions can be checked here.

  $ export HOME=$PWD
  $ refresh() { day10 refresh-base --cache-dir cache "$@"; }

Nothing to refresh when the platform has no base image, and that is not an
error: a builder that has never built for a platform has nothing stale.

  $ mkdir -p cache
  $ refresh
  [WARNING] No base image to refresh in cache
  $ refresh --max-age 24
  [WARNING] No base image to refresh in cache

An index refreshed recently is left alone, so this is safe to call on every
idle window rather than only when something is known to be stale.

  $ mkdir -p cache/debian-13-x86_64/base
  $ touch cache/debian-13-x86_64/base/build.log
  $ refresh --max-age 24
  [NOTE] debian-13-x86_64: refreshed 0 hour(s) ago, within 24

Every platform in the cache, not only the one this machine happens to be: a
builder serving the whole matrix has nineteen, and the one that goes stale
unnoticed is the one nobody thought to name.

  $ for p in alpine-3.24-x86_64 debian-testing-x86_64 opensuse-tumbleweed-x86_64; do
  >   mkdir -p cache/$p/base && touch cache/$p/base/build.log
  > done
  $ refresh --max-age 24
  [NOTE] alpine-3.24-x86_64: refreshed 0 hour(s) ago, within 24
  [NOTE] debian-13-x86_64: refreshed 0 hour(s) ago, within 24
  [NOTE] debian-testing-x86_64: refreshed 0 hour(s) ago, within 24
  [NOTE] opensuse-tumbleweed-x86_64: refreshed 0 hour(s) ago, within 24

Naming one narrows it, the same way it does for cache-info and prune.

  $ refresh --max-age 24 --os-distribution debian
  [NOTE] debian-13-x86_64: refreshed 0 hour(s) ago, within 24
  [NOTE] debian-testing-x86_64: refreshed 0 hour(s) ago, within 24

  $ refresh --max-age 24 --arch x86_64 --os-version tumbleweed
  [NOTE] opensuse-tumbleweed-x86_64: refreshed 0 hour(s) ago, within 24

A directory that is not a layer cache is left alone rather than guessed at.

  $ mkdir -p cache/nonsense/base
  $ refresh --max-age 24 --os-distribution nonsense
  [WARNING] No base image to refresh in cache
