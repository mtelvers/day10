FROM --platform=linux/amd64 opensuse/leap:15.6 AS opam-builder
RUN zypper refresh && zypper install -y gcc make patch unzip bzip2 tar git curl sudo diffutils findutils libcap-devel gzip
RUN git clone --depth 1 --branch 2.4.1 https://github.com/ocaml/opam.git /tmp/opam
WORKDIR /tmp/opam
RUN make cold
RUN make install

FROM --platform=linux/amd64 opensuse/leap:15.6 AS opam-build-builder
RUN zypper refresh && zypper install -y gcc make patch unzip bzip2 tar git curl diffutils findutils gzip
COPY --from=opam-builder [ "/usr/local/bin/opam", "/usr/local/bin/opam" ]
RUN opam init --disable-sandboxing -a --bare -y
RUN git clone --depth 1 --branch master https://github.com/mtelvers/opam-build.git /tmp/opam-build
WORKDIR /tmp/opam-build
RUN opam switch create . 5.3.0 --deps-only -y
RUN opam exec -- dune build --release
RUN install -m 755 _build/default/bin/main.exe /usr/local/bin/opam-build

FROM --platform=linux/amd64 opensuse/leap:15.6
RUN zypper refresh && zypper update -y
RUN zypper install -y gcc gcc-c++ make patch unzip bzip2 tar xz git curl sudo rsync diffutils findutils m4 gawk which gzip
COPY --from=opam-builder [ "/usr/local/bin/opam", "/usr/local/bin/opam" ]
COPY --from=opam-build-builder [ "/usr/local/bin/opam-build", "/usr/local/bin/opam-build" ]
RUN if getent passwd 1000; then userdel -r $(id -nu 1000); fi
RUN groupadd --gid 1000 opam
RUN useradd --uid 1000 --gid 1000 --home-dir /home/opam --create-home --shell /bin/bash opam
RUN chown -R 1000:1000 /home/opam
RUN echo 'opam ALL=(ALL:ALL) NOPASSWD:ALL' > /etc/sudoers.d/opam
RUN chmod 440 /etc/sudoers.d/opam
RUN chown root:root /etc/sudoers.d/opam
COPY --chown=1000:1000 [ "opam-repository", "/home/opam/opam-repository" ]
USER 1000:1000
WORKDIR /home/opam
RUN opam init -k local -a /home/opam/opam-repository --bare --disable-sandboxing -y
RUN opam switch create default --empty
