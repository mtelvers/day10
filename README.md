
```
./_build/install/default/bin/day10 health-check --cache-dir /home/mtelvers/cache/ --opam-repository /home/mtelvers/opam-repository 0install.2.18
./_build/install/default/bin/day10 health-check --cache-dir /home/mtelvers/cache/ --opam-repository /home/mtelvers/opam-repository obuilder.0.6.0
./_build/install/default/bin/day10 health-check --cache-dir /home/mtelvers/cache/ --opam-repository /home/mtelvers/opam-repository cohttp.6.1.0
./_build/install/default/bin/day10 health-check --cache-dir /home/mtelvers/cache/ --opam-repository /home/mtelvers/opam-repository odoc.3.0.0
```

```
./_build/install/default/bin/day10 ci --cache-dir /home/mtelvers/cache/ --opam-repository /home/mtelvers/opam-repository /home/mtelvers/day10
```


Windows

```
winget install git.git --scope machine
winget install ocaml.opam --scope machine
```

```
dune build
_build\install\default\bin\day10.exe health-check --cache-dir c:\Users\Administrator\cache --opam-repository c:\Users\Administrator\opam-repository 0install.2.18
```

```
make -j 2 SYSTEM=windows-x86_64 OUTPUT_DIR=./output CACHE_DIR=c:\\Users\\Administrator\\cache OPAM_REPO=c:\\Users\\Administrator\\opam-repository all
```
