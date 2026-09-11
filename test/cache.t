Reporting on a cache and pruning it need neither a container nor a repository,
so they can be driven here directly.

  $ export HOME=$PWD

Two platforms: one nothing has wanted for months, one in daily use.  A layer is
a directory holding a layer.json, whose mtime is when the layer was last used.

The ages are deliberately away from any bucket boundary.  An age that lands on
one -- three days exactly, against twelve hour buckets -- would fall either side
of it depending on how long the test took to get here, and the table would
differ between runs.

  $ for d in debian-13-riscv64/old1 debian-13-riscv64/old2 debian-13-riscv64/old3; do
  >   mkdir -p cache/$d
  >   echo '{"package":"p.1","exit_status":0}' > cache/$d/layer.json
  >   touch -d '100 days ago' cache/$d/layer.json
  > done
  $ for d in ubuntu-24.04-x86_64/hot1 ubuntu-24.04-x86_64/hot2; do
  >   mkdir -p cache/$d
  >   echo '{"package":"q.1","exit_status":0}' > cache/$d/layer.json
  >   touch -d '80 hours ago' cache/$d/layer.json
  > done

A failed layer is counted apart from the rest.

  $ mkdir -p cache/ubuntu-24.04-x86_64/broken
  $ echo '{"package":"r.1","exit_status":1}' > cache/ubuntu-24.04-x86_64/broken/layer.json
  $ touch -d '80 hours ago' cache/ubuntu-24.04-x86_64/broken/layer.json

Neither of these is a layer: base/ holds no layer.json, and a temp directory
belongs to a run in progress.  Both must stay out of every total, and out of
prune's reach.

  $ mkdir -p cache/ubuntu-24.04-x86_64/base cache/temp-abc123
  $ echo not-a-layer > cache/ubuntu-24.04-x86_64/base/build.log

Each layer records its size the first time it is measured, so the first report
walks the cache and says so on stderr.  Discard that run, then the output is
read from the records and is stable.

  $ day10 cache-info --cache-dir cache > /dev/null 2>&1
  $ cp -a cache prune-days
  $ cp -a cache prune-percent
  $ cp -a cache prune-size

  $ day10 cache-info --cache-dir cache 2>/dev/null
  
  debian-13-riscv64          3 layers     24.0K
    days since last used             size   layers  (oldest 14 weeks)
       0- 14                         0.0B        0
      14- 28                         0.0B        0
      28- 42                         0.0B        0
      42- 56                         0.0B        0
      56- 70                         0.0B        0
      70- 84                         0.0B        0
      84- 98                         0.0B        0
      98-112  ##################    24.0K        3
  
  ubuntu-24.04-x86_64        3 layers     24.0K   1 failed
    hours since last used            size   layers
       0- 12                         0.0B        0
      12- 24                         0.0B        0
      24- 36                         0.0B        0
      36- 48                         0.0B        0
      48- 60                         0.0B        0
      60- 72                         0.0B        0
      72- 84  ##################    24.0K        3
  
  total, 2 platforms         6 layers     48.0K   1 failed
  
    would free   --days 90         24.0K      3 layers
                 --days 30         24.0K      3 layers
                 --percent 90       8.0K      1 layer
                 --percent 50      24.0K      3 layers

Prune covers every platform and ranks across all of them, so a cutoff takes the
stale platform and leaves the busy one alone.

  $ day10 prune --cache-dir prune-days --days 50 2>&1
  [NOTE] Pruning 3 cache entries (unused for more than 50 day(s)): debian-13-riscv64 3
  [NOTE] Freed 24.0K
  $ ls prune-days/debian-13-riscv64 prune-days/ubuntu-24.04-x86_64
  prune-days/debian-13-riscv64:
  
  prune-days/ubuntu-24.04-x86_64:
  base
  broken
  hot1
  hot2

base/ and the temp directory are untouched.

  $ ls -d prune-days/ubuntu-24.04-x86_64/base prune-days/temp-abc123
  prune-days/temp-abc123
  prune-days/ubuntu-24.04-x86_64/base

A percentage is of the size, not of the number of entries.

  $ day10 prune --cache-dir prune-percent --percent 50 2>&1
  [NOTE] Pruning 3 cache entries (keeping the newest 50% of 48.0K, so 24.0K): debian-13-riscv64 3
  [NOTE] Freed 24.0K

A ceiling is absolute, and wants a unit: a bare number would read as that many
bytes and empty the cache.

  $ day10 prune --cache-dir prune-size --max-size 24K 2>&1
  [NOTE] Pruning 3 cache entries (24.0K over 24.0K): debian-13-riscv64 3
  [NOTE] Freed 24.0K
  $ day10 prune --cache-dir prune-size --max-size 40
  [ERROR] --max-size wants a size with a unit, such as 40G or 500M, not "40"
  [1]

The modes are mutually exclusive, and one is required.

  $ day10 prune --cache-dir prune-size --days 1 --percent 50
  [ERROR] --days, --percent and --max-size are mutually exclusive
  [1]
  $ day10 prune --cache-dir prune-size
  [ERROR] Specify one of --days N, --percent N or --max-size SIZE
  [1]

A platform filter narrows what is considered, so a cutoff that would have taken
the stale platform takes nothing.

  $ day10 prune --cache-dir prune-days --days 50 --arch x86_64 2>&1
  [NOTE] No cache entries to prune (unused for more than 50 day(s))

An empty cache says so rather than printing an empty table.

  $ mkdir empty
  $ day10 cache-info --cache-dir empty
  
  Nothing in empty
