FROM --platform=linux/amd64 alpine:3.22 AS opam-builder
RUN apk update && apk add build-base patch unzip bzip2 tar git curl openssl sudo linux-headers libcap-dev
RUN git clone --depth 1 --branch 2.4.1 https://github.com/ocaml/opam.git /tmp/opam
WORKDIR /tmp/opam
RUN make cold
RUN make install

FROM --platform=linux/amd64 alpine:3.22 AS day10-install-builder
RUN apk update && apk add build-base patch unzip bzip2 tar git curl bubblewrap
COPY --from=opam-builder [ "/usr/local/bin/opam", "/usr/local/bin/opam" ]
RUN opam init --disable-sandboxing -a --bare -y
ADD [ "https://api.github.com/repos/mtelvers/day10-install/git/refs/heads/master", "/tmp/day10-install.ref" ]
RUN git clone --depth 1 --branch master https://github.com/mtelvers/day10-install.git /tmp/day10-install
WORKDIR /tmp/day10-install
RUN opam switch create . 5.3.0 --deps-only -y
RUN opam exec -- dune build --release
RUN install -m 755 _build/default/bin/main.exe /usr/local/bin/day10-install

FROM --platform=linux/amd64 alpine:3.22
RUN apk update && apk upgrade && apk add build-base patch unzip bzip2 tar xz git curl sudo rsync bash coreutils diffutils bubblewrap
COPY --from=opam-builder [ "/usr/local/bin/opam", "/usr/local/bin/opam" ]
COPY --from=day10-install-builder [ "/usr/local/bin/day10-install", "/usr/local/bin/day10-install" ]
RUN addgroup -g 1000 opam
RUN adduser -D -u 1000 -G opam -h /home/opam -s /bin/sh opam
RUN chown -R 1000:1000 /home/opam
RUN echo 'opam ALL=(ALL:ALL) NOPASSWD:ALL' > /etc/sudoers.d/opam
RUN chmod 440 /etc/sudoers.d/opam
RUN chown root:root /etc/sudoers.d/opam
COPY --chown=1000:1000 [ "opam-repository", "/home/opam/opam-repository" ]
USER 1000:1000
WORKDIR /home/opam
RUN opam init -k local -a /home/opam/opam-repository --bare --disable-sandboxing -y
RUN echo 'archive-mirrors: "https://opam.ocaml.org/cache"' >> /home/opam/.opam/config
RUN opam switch create default --empty
