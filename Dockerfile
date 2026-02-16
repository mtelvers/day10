ARG BASE_OS=debian
ARG BASE_VERSION=13
FROM ocaml/opam:${BASE_OS}-${BASE_VERSION}-ocaml-5.3 AS build
RUN sudo ln -f /usr/bin/opam-2.3 /usr/bin/opam && opam init --reinit -ni
RUN sudo apt update && sudo apt install libgmp-dev libffi-dev pkg-config m4 -y --no-install-recommends
RUN cd ~/opam-repository && git fetch origin master && opam update
COPY --chown=opam day10.opam /src/
WORKDIR /src
RUN opam install -y --deps-only .
ADD --chown=opam . .
RUN opam exec -- dune build @install

ARG BASE_OS=debian
ARG BASE_VERSION=13
FROM ${BASE_OS}:${BASE_VERSION}
WORKDIR /app
COPY --from=build /src/_build/install/default/bin/day10 /usr/local/bin/
ENTRYPOINT ["/usr/local/bin/day10"]
