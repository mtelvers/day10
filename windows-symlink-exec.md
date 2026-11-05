# Windows containers cannot execute a relative symlink on a bind mount

Seen 5 November 2026 on optiplex (Windows Server 2025 build 26100, containerd
2.1.3, process isolation). ocaml-compiler.5.5.2 fails under day10's Windows
backend at the link of ocamlc.opt.exe:

    LINKOPT ocamlc.opt.exe
    The system cannot find the file C:\Users\ContainerAdministrator\AppData\Local\opam\default\.opam-switch\build\ocaml-compiler.5.5.2\opt\bin\flexlink.exe.

`opt/bin/flexlink.exe` is a native relative symlink to `../../flexlink.opt.exe`,
made by `cd opt/bin; ln -sf ...` under `CYGWIN=winsymlinks:nativestrict`, which
OCaml's Makefile.common exports.  The link is there and reads fine; executing
it from a native program fails.  OCaml 4.14 and 5.3 are unaffected because
their configure uses `cp -pf` instead (the link came with ocaml/ocaml#13494,
first released in 5.4.0).

## Reproduction, no Cygwin or OCaml needed

On the host:

    mkdir D:\symtest\a\b
    copy C:\Windows\System32\whoami.exe D:\symtest\t.exe
    echo hello> D:\symtest\t.txt

Inside a container with that directory bind-mounted:

    ctr run --rm --mount type=bind,src=D:\symtest,dst=C:\symtest,options=rbind:rw ^
      mcr.microsoft.com/windows/servercore:ltsc2025 symtest ^
      cmd /c "cd /d C:\symtest\a\b && mklink l.txt ..\..\t.txt && type l.txt && mklink l.exe ..\..\t.exe && l.exe"

    hello
    The system cannot find the file C:\symtest\a\b\l.exe.

What does and does not work:

| Link                          | Where                        | Read | Execute |
|-------------------------------|------------------------------|------|---------|
| relative `..\..\t.exe`        | bind mount from D: (ReFS)    | yes  | no      |
| relative `..\..\t.exe`        | bind mount from C: (NTFS)    | yes  | no      |
| relative, host path == container path | bind mount           | yes  | no      |
| absolute `C:\symtest\t.exe`   | bind mount                   | yes  | yes     |
| relative `..\..\t.exe`        | container's own filesystem   | yes  | yes     |
| relative `..\..\t.exe`        | on the host, no container    | yes  | yes     |

No mount option changes this: for a process-isolated container hcsshim reads
only `ro` from the mount options and passes the directory to HCS as a
MappedDirectory, which has no other settings.
