FROM --platform=linux/amd64 debian:13 AS opam-builder
RUN apt update && apt install -y build-essential git curl libcap-dev sudo
RUN git clone --depth 1 --branch 2.4.1 https://github.com/ocaml/opam.git /tmp/opam
WORKDIR /tmp/opam
RUN make cold
RUN make install

FROM --platform=linux/amd64 debian:13 AS day10-install-builder
RUN apt update && apt install -y build-essential git curl unzip bubblewrap
COPY --from=opam-builder [ "/usr/local/bin/opam", "/usr/local/bin/opam" ]
RUN opam init --disable-sandboxing -a --bare -y
ADD [ "https://api.github.com/repos/mtelvers/day10-install/git/refs/heads/master", "/tmp/day10-install.ref" ]
RUN git clone --depth 1 --branch master https://github.com/mtelvers/day10-install.git /tmp/day10-install
WORKDIR /tmp/day10-install
RUN opam switch create . 5.3.0 --deps-only -y
RUN opam exec -- dune build --release
RUN install -m 755 _build/default/bin/main.exe /usr/local/bin/day10-install

FROM --platform=linux/amd64 debian:13
RUN apt update && apt upgrade -y && apt install -y build-essential curl git rsync sudo unzip nano libcap-dev libx11-dev bubblewrap
COPY --from=opam-builder [ "/usr/local/bin/opam", "/usr/local/bin/opam" ]
COPY --from=day10-install-builder [ "/usr/local/bin/day10-install", "/usr/local/bin/day10-install" ]
RUN echo 'debconf debconf/frontend select Noninteractive' | debconf-set-selections
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
RUN echo 'archive-mirrors: "https://opam.ocaml.org/cache"' >> /home/opam/.opam/config
RUN opam switch create default --empty
