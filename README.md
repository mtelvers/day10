
```
./_build/install/default/bin/day10 health-check --cache-dir /home/mtelvers/cache/ --opam-repository /home/mtelvers/opam-repository 0install.2.18
./_build/install/default/bin/day10 health-check --cache-dir /home/mtelvers/cache/ --opam-repository /home/mtelvers/opam-repository obuilder.0.6.0
./_build/install/default/bin/day10 health-check --cache-dir /home/mtelvers/cache/ --opam-repository /home/mtelvers/opam-repository cohttp.6.1.0
./_build/install/default/bin/day10 health-check --cache-dir /home/mtelvers/cache/ --opam-repository /home/mtelvers/opam-repository odoc.3.0.0
```

```
./_build/install/default/bin/day10 ci --cache-dir /home/mtelvers/cache/ --opam-repository /home/mtelvers/opam-repository /home/mtelvers/day10
```


# Windows

Remove Windows Defender

```
dism /online /disable-feature /featurename:Windows-Defender /remove /norestart
```

Install OpenSSH and configure (Windows Server 2022 only)

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

Install Git and ensure you restart your shell before continuing.

```
curl.exe -L https://github.com/git-for-windows/git/releases/download/v2.50.0.windows.1/Git-2.50.0-64-bit.exe -o c:\windows\temp\git.exe
start /wait c:\windows\temp\git.exe /VERYSILENT /NORESTART /NOCANCEL /SP- /CLOSEAPPLICATIONS /RESTARTAPPLICATIONS /TASKS="addtopath"
```

Install Containerd. On the last line selection `ltsc2025` if using Windows Server 2025.

```
curl.exe https://raw.githubusercontent.com/microsoft/Windows-Containers/refs/heads/Main/helpful_tools/Install-ContainerdRuntime/install-containerd-runtime.ps1 -o install-containerd-runtime.ps1
Set-ExecutionPolicy Bypass
.\install-containerd-runtime.ps1 -ContainerDVersion 2.1.3 -WinCNIVersion 0.3.1 -ExternalNetAdapter Ethernet -ContainerBaseImage mcr.microsoft.com/windows/servercore:ltsc2022
```

Create `C:\Program Files\containerd\cni\conf\0-containerd-nat.conf` containing

```
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

Install opam

```
curl.exe -L https://github.com/ocaml/opam/releases/download/2.3.0/opam-2.3.0-x86_64-windows.exe -o c:\windows\opam.exe
opam init -y
```

Download and build mtelvers/hcn-namespace

```
git clone https://github.com/mtelvers/hcn-namespace
cd hcn-namespace
opam install . --deps-only
for /f "tokens=*" %i in ('opam env') do @%i
dune build
copy _build\install\default\bin\hcn-namespace.exe %LocalAppData%\opam\.cygwin\root\usr\local\bin
```

Build this project

```
git clone https://github.com/mtelvers/ohc -b tool
cd ohc
opam install . --deps-only
dune build
```

Run

```
git clone http://github.com/ocaml/opam-repository c:\opam-repository
mkdir c:\cache
make -j 6 SYSTEM=windows-x86_64 OUTPUT_DIR=./output CACHE_DIR=c:\\cache OPAM_REPO=c:\\opam-repository all
```




Next commit

```
NEXT_MERGE=$(git rev-list --merges --reverse HEAD..upstream/master | head -1)
git checkout $NEXT_MERGE
```

