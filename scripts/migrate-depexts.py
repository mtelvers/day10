#!/usr/bin/env python3
"""Record in each layer's layer.json whether it installed system packages.

day10 writes "depexts" into layer.json as it builds a layer, and prune
--depexts and --base read it from there.  A layer built before the field
existed has to be given it once, by the same test the build used to make:
whether a package manager's database is in the layer's tree.  Run this over
every Linux cache once, after upgrading day10:

    migrate-depexts.py CACHE_DIR...

layer.json's mtime is when the layer was last used, which prune sorts on, so
it is put back after the rewrite.  Layers already carrying the field are left
alone, so the script can be run again.
"""

import json
import os
import sys

# One per package manager: dpkg's status file, rpm's database in either of its
# homes, apk's installed file, pacman's local directory.  Only Linux caches
# predate the field.
DATABASES = [
    "var/lib/dpkg/status",
    "var/lib/rpm",
    "usr/lib/sysimage/rpm",
    "lib/apk/db/installed",
    "var/lib/pacman/local",
]


def migrate(cache):
    done = skipped = 0
    for platform in sorted(os.listdir(cache)):
        if not os.path.isdir(os.path.join(cache, platform, "base", "fs")):
            continue
        for layer in sorted(os.listdir(os.path.join(cache, platform))):
            path = os.path.join(cache, platform, layer, "layer.json")
            if not os.path.isfile(path):
                continue
            with open(path) as f:
                info = json.load(f)
            if "depexts" in info:
                skipped += 1
                continue
            tree = os.path.join(cache, platform, layer, "fs")
            info["depexts"] = any(os.path.exists(os.path.join(tree, p)) for p in DATABASES)
            stat = os.stat(path)
            with open(path, "w") as f:
                json.dump(info, f)
            os.utime(path, (stat.st_atime, stat.st_mtime))
            done += 1
    print(f"{cache}: {done} layer(s) recorded, {skipped} already had the field")


if __name__ == "__main__":
    if len(sys.argv) < 2:
        sys.exit(__doc__)
    for cache in sys.argv[1:]:
        migrate(cache)
