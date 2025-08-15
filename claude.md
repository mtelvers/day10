Hi Claude, please can you write a JavaScript application which will run in a web browser to display the results of CI builds.

A directory structure exists as shown below, which includes the GitHub SHA used in the run and a list of directories below, one per OS where it was tested. Not all the testing happens in parallel, so potentially some OS directories won't be available. Thus, don't assume there will always be Debian and Windows - there may be others or fewer. Below the OS directory is the version of the compiler used for testing, in this case, version 5.3.0.

```
.
├── commits.json
└── d2563c38bd32daaee47b6a6cade7e4c79270ef73
    ├── commit.json
    └── debian-12
        └── 5.3.0
            ├── 0install.2.18.dot
            ├── 0install.2.18.json
            ├── alcotest.1.9.0.dot
            ├── alcotest.1.9.0.json
            ├── ansi.0.7.0.dot
            ├── ansi.0.7.0.json
            ├── bos.0.2.1.dot
            ├── bos.0.2.1.json
            ├── diffast-api.0.2.dot
            └── diffast-api.0.2.json
```

`commits.json` contains a description of each commit directory like this

```
[
  {
    "sha": "d2563c38bd32daaee47b6a6cade7e4c79270ef73",
    "date": "2025-07-24T03:50:08+00:00",
    "message": "Merge pull request #28190 from rmonat/opam-publish-mopsa.1.2"
  }
]
```

And each `commit.json` contains this:

```
{
  "debian-12": {
    "5.3.0": [
      {
        "name": "0install.2.18",
        "status": "success",
        "layer": "a900bb178c94aec3a9b6be96dc150ddc"
      },
      {
        "name": "alcotest.1.9.0",
        "status": "success",
        "layer": "9fcc87163d8aaf7985a90210c0ef37b1"
      },
      {
        "name": "ansi.0.7.0",
        "status": "success",
        "layer": "336e1b50c3aeab32120df1621c3e1cee"
      },
      {
        "name": "bos.0.2.1",
        "status": "success",
        "layer": "66d17ff760cb09b4217f4b392b96c792"
      },
      {
        "name": "diffast-api.0.2",
        "status": "success",
        "layer": "85314b19757c6d8df1f602451071eea8"
      }
    ]
  }
}
```

`name` contains the name of the package. The package name is in the format name dot version. So 0install.2.18.1 is package 0install version 2.15.1. i.e. The first occurrence of a dot indicates the start of the version number. The version number itself can contain many dots.

`status` is the overall status of the build, which will be one of: no_solution, dependency_failed, failure, success.

The `.dot` files are GraphViz file with the same name as the package. Where there is a solution a `.dot` file is provided but not all packages have a solution. Please add the digraph to the package page log. Let's render it the browser with `Viz.js`. The output will need to be scaled to fit the display window as the graph may be quite large. The `.dot` files are minimal without any styling. Example below:

```
digraph opam {
  "0install-solver.2.18" -> {"dune.3.19.1" "ocaml.5.3.0"}
  "dune.3.19.1" -> {"base-threads.base" "base-unix.base" "ocaml.5.3.0"}
  "ocaml.5.3.0" -> {"ocaml-base-compiler.5.3.0" "ocaml-config.3"}
  "ocaml-base-compiler.5.3.0" -> "ocaml-compiler.5.3.0";
  "ocaml-config.3" -> "ocaml-base-compiler.5.3.0";
}
```

When we render these, we should apply some nice default styling like this:

```js
const defaultStyles = { rankdir: "LR", nodeShape: "box", nodeFontColor: "#ffffff", nodeColor: "#ef7a08", nodeFillColor: "#ef7a08", nodeStyle: "filled", edgeColor: "#888888" };
```

The `layer` is a hash of the layer which contains this build.

The hash index into the `/cache/` directory structure:

```
/cache
├── fc3a8cbcba91cf5d11de21dad7d138bc
│   ├── build.log
│   ├── layer.json
├── adad97c884045a672843d4de9980f82d
│   ├── build.log
│   ├── layer.json
```

`build.log` is the text output of the build.

`layer.json` is a JSON file containing the package name for this layer. `deps` is a list of direct dependency packages, `created` is the Unix timestamp of when the layer was created, and `status` contains a integer value, which is the exit code of that step. 0 = success, anything else is failure.

`hashes` is a _complete_ list of all dependent layers, which are indexes into the `/cache` structure.

We should NOT load the `/cache/layers.json` from each dependent layer. The initial `/cache/layers.json` contains ALL of the dependent layers.

The deps array contains the package names, and the hashes array contains the corresponding layer hashes, and they're in the same order. This means deps[0] corresponds to hashes[0], deps[1] corresponds to hashes[1], etc. Therefore, when displaying a sub layer, we can use the name from `deps[n]` rather than displaying `hashes[n]`.

```
{{"package":"0install.2.18","exit_status":0,"deps":["ocurl.0.9.2","obus.1.2.5","lwt_react.1.2.0","lwt_ppx.5.9.1","lwt_log.1.1.2","lwt.5.9.1","xmlm.1.4.0","sha.1.15.4","react.1.2.2","ppxlib.0.35.0","ocplib-endian.1.2","menhir.20240715","dune-configurator.3.19.1","yojson.3.0.0","topkg.1.0.8","stdlib-shims.0.3.0","sexplib0.v0.17.0","ppx_derivers.1.2.1","ocaml-compiler-libs.v0.17.0","menhirSdk.20240715","menhirLib.20240715","menhirCST.20240715","csexp.1.5.2","cppo.1.8.0","base-bytes.base","0install-solver.2.18","ocamlfind.1.9.8","ocamlbuild.0.16.1","dune.3.19.1","ocaml.5.3.0","ocaml-config.3","ocaml-base-compiler.5.3.0","ocaml-compiler.5.3.0","conf-libcurl.2","base-unix.base","base-threads.base"],"hashes":["13ab638dcd860284863fd3a789868bac","936ac8e89f1781872af41c9780e3d421","2b83dd96d968dd921d6c53fb5d95cafc","d3c7cd833ee0e6b3fc08775ff9e82138","36ef6c2ba31e9c4ab5105a434d442ef4","08408fb34675182de9f456e0e39d0d47","19659791294010cc8d9cbd4b38f7e05b","2f9417ef8a4aedde0ba7cbc20f2635ce","edabe71adfdd082786455a37eefd5ade","2a47480c55f1c296bea55aea7d517132","d5551810d57c96eb0efc23e34b2a2d85","02ad9577d22698f5b0eeafc805566937","7ba2da9b3e919b0ec46c2885408b5a13","c111d0a4ce2c437af31c375d8310364c","2959afacd302f3852f1b203975b20442","e44f794eb6b75ee3f0873f8d44053640","2ba84bac5dd568cbe4e347eea0cb5c04","636f22552f4f47d53b0acf51c32dc002","95cacb84e95b32f1e09eec2ac8235876","aaad39ce113f3211ea5f6ac2949c263f","187beb9820c9e7e3983abd78b0d7c26c","907742fd086bef00728418b60ab1b1eb","da4cb5bcd209a33dfaef4609dd59fcf5","fd2ec950f6ec57ee9f3068a1be983eb2","6ce5d36394264c11fa9c9783f5c571eb","a867fb905efb9b7c28d2d93e911262bf","2502449a08ffae10ec7e76f69573bea0","ffe3a3b5cdff0845a333205c23109703","76659bcdbdbfff426a73ebb9b286a4d2","194f6e5c7f38885c9e2eea57d37ce7b0","351d01c1b782e729cf6a322649908629","71c33cbf6b90df69ede09d910b6d2a53","1980e378e8e2321c71032d0d6a8aa32d","9d23d01aec9788fe82d6b9f7a9aac11e","b102fe18cbdaa1a210b1fce3dbf1acbc","709149b2baf4d5f8851c4c1fe0cc5512"],"created":1754905764.0}
```

The site layout would be like this:-

```
site/
├── cache
│   ├── 061bae6b4dbdb04ae77b8bb4f22d9a35
│   │   └── layer.json
│   └── 07958b7376fc56c89e5838b1dac502db
│       └── layer.json
├── ce03608b4ba656c052ef5e868cf34b9e86d02aac
│   └── commit.json
└── commits.json
├── index.html       # generate this
├── script.js        # generate this
└── stylesheet.css   # generate this
```

`index.html` would load `script.js` and `stylesheet.css` to display the site although these can be embedded in a single `index.html` if preferred.

The land page would display a list of commits from `commits.json`. Each of these could be clicked on to display a list of the packages within that commit.

The packages should be displayed as a table with rows for each package, and columns for each compiler and a colour-coded link to the build log. We must use symbols of `success`, `failure`, `no_solution` and `dependency_failed` to save on space. Each OS should be represented as a series of tabs along the top of the page. The page should cope with a narrow display on a mobile phone to avoid losing the right-hand columns of the table. Perhaps the ability to scroll would be sufficient. Since there will be ~5000 rows and 12 columns in the table, we should implement pagination.

For any given package, we should display the build log and the GraphViz visualisation as separate tabs. These graphs can be quite large so we might need to be able to zoom.

The build log should lazy-load all of the sub-layer build logs.

Can make it so that the page URL reflects the current page to provide a permalink to the current commit/os/package?

