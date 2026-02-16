# day10

A tool for health-checking and CI-testing OCaml packages from the opam repository.

Packages are build inside inside lightweight containers (runc on Linux, jails on FreeBSD, containerd on Windows). opam packages are built in isolation and subsequently merged to from a switch with the given packages. Caching each build as a layer for efficient incremental rebuilds.

## Building

```
opam install . --deps-only
dune build @install
```

Or with Docker:

```
docker build -t day10 .
```

## Usage

### Health-check a package

```
day10 health-check --cache-dir /path/to/cache --opam-repository /path/to/opam-repository 0install.2.18
```

### Health-check multiple packages

Create a JSON file (`packages.json`) containing `{"packages":["pkg1.v1","pkg2.v2",...]}`, then:

```
day10 health-check --cache-dir /path/to/cache --opam-repository /path/to/opam-repository @packages.json
```

Use `--fork N` to build packages in parallel with N worker processes.

### CI test a local project

```
day10 ci --cache-dir /path/to/cache --opam-repository /path/to/opam-repository /path/to/project
```

### List available packages

```
day10 list --opam-repository /path/to/opam-repository
```

Add `--all-versions` to list every version rather than just the latest.

### Common options

| Option | Description |
|---|---|
| `--cache-dir DIR` | Directory for build layer cache (required) |
| `--opam-repository DIR` | Path to opam repository (required, repeatable) |
| `--ocaml-version VERSION` | OCaml compiler version (default `ocaml.5.3.0`) |
| `--json FILE` | Write results as JSON |
| `--md FILE` | Write results as Markdown |
| `--dot FILE` | Write dependency graph in Graphviz DOT format |
| `--tag TAG` | Import built layers into Docker with the given tag |
| `--with-test` | Enable test dependencies |
| `--dry-run` | Check if layers exist without building |
| `--log` | Print build logs to stdout |
| `--fork N` | Run N builds in parallel |

## Platform setup

### Linux

Linux builds use **runc** to run each package build in an OCI container with an overlay filesystem. The base layer is a Debian image built with Docker.

#### Prerequisites

- Docker (for building the base layer)
- runc
- sudo access (for overlay mounts and runc invocation)

#### Install runc

On Debian/Ubuntu:

```
apt install runc
```

#### Clone the opam repository

```
git clone https://github.com/ocaml/opam-repository /home/mtelvers/opam-repository
```

#### Build and run

```
opam install . --deps-only
dune build @install
./_build/install/default/bin/day10 health-check \
  --cache-dir /home/mtelvers/cache \
  --opam-repository /home/mtelvers/opam-repository \
  0install.2.18
```

The first run builds a base Debian container image via Docker, exports it as a root filesystem, and caches it. Subsequent runs reuse the cached base layer.

To run batch health-checks:

```
make -j$(nproc) all
```

### FreeBSD

FreeBSD builds use **jails** with nullfs and unionfs mounts for filesystem layering. The base layer is a FreeBSD 14.2-RELEASE system installed via `bsdinstall`.

#### Clone the opam repository

```
git clone https://github.com/ocaml/opam-repository /home/mtelvers/opam-repository
```

#### Build and run

```
opam install . --deps-only
dune build @install
./_build/install/default/bin/day10 health-check \
  --cache-dir /home/mtelvers/cache \
  --opam-repository /home/mtelvers/opam-repository \
  0install.2.18
```

The first run downloads the FreeBSD 14.2-RELEASE base system, installs opam and opam-build into the jail, creates an opam user, and caches the result as the base layer.

### Windows

Windows builds use **containerd** with Windows containers.

#### Remove Windows Defender

```
dism /online /disable-feature /featurename:Windows-Defender /remove /norestart
```

#### Install OpenSSH and configure (Windows Server 2022 only)

```
curl.exe -L https://github.com/PowerShell/Win32-OpenSSH/releases/download/v9.2.2.0p1-Beta/OpenSSH-Win64-v9.2.2.0.msi -o openssh-win64.msi
start /wait msiexec /q /norestart /i openssh-win64.msi
copy id_ed25519.pub c:\programdata\ssh\administrators_authorized_keys
netsh advfirewall firewall set rule name="OpenSSH SSH Server Preview (sshd)" new profile=any enable=yes
```

On Windows Server 2025, SSHD is already installed, but not enabled.

```
sc config sshd start=auto
net start sshd
copy id_ed25519.pub c:\programdata\ssh\administrators_authorized_keys
netsh advfirewall firewall set rule name="OpenSSH SSH Server (sshd)" new profile=any enable=yes
```

#### Install Git and ensure you restart your shell before continuing

```
curl.exe -L https://github.com/git-for-windows/git/releases/download/v2.50.0.windows.1/Git-2.50.0-64-bit.exe -o c:\windows\temp\git.exe
start /wait c:\windows\temp\git.exe /VERYSILENT /NORESTART /NOCANCEL /SP- /CLOSEAPPLICATIONS /RESTARTAPPLICATIONS /TASKS="addtopath"
```

#### Install Containerd

On the last line select `ltsc2025` if using Windows Server 2025.

```
curl.exe https://raw.githubusercontent.com/microsoft/Windows-Containers/refs/heads/Main/helpful_tools/Install-ContainerdRuntime/install-containerd-runtime.ps1 -o install-containerd-runtime.ps1
Set-ExecutionPolicy Bypass
.\install-containerd-runtime.ps1 -ContainerDVersion 2.1.3 -WinCNIVersion 0.3.1 -ExternalNetAdapter Ethernet -ContainerBaseImage mcr.microsoft.com/windows/servercore:ltsc2022
```

Create `C:\Program Files\containerd\cni\conf\0-containerd-nat.conf` containing:

```json
{
    "cniVersion": "0.3.0",
    "name": "nat",
    "type": "nat",
    "master": "Ethernet",
    "ipam": {
        "subnet": "172.20.0.0/16",
        "routes": [
            {
                "gateway": "172.20.0.1"
            }
        ]
    },
    "capabilities": {
        "portMappings": true,
        "dns": true
    }
}
```

#### Install opam

```
curl.exe -L https://github.com/ocaml/opam/releases/download/2.3.0/opam-2.3.0-x86_64-windows.exe -o c:\windows\opam.exe
opam init -y
```

#### Download and build mtelvers/hcn-namespace

```
git clone https://github.com/mtelvers/hcn-namespace
cd hcn-namespace
opam install . --deps-only
for /f "tokens=*" %i in ('opam env') do @%i
dune build
copy _build\install\default\bin\hcn-namespace.exe %LocalAppData%\opam\.cygwin\root\usr\local\bin
```

#### Build this project

```
git clone https://github.com/mtelvers/day10 -b tool
cd day10
opam install . --deps-only
dune build
```

#### Run

```
git clone http://github.com/ocaml/opam-repository c:\opam-repository
mkdir c:\cache
make -j 6 SYSTEM=windows-x86_64 OUTPUT_DIR=./output CACHE_DIR=c:\\cache OPAM_REPO=c:\\opam-repository all
```
