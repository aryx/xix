# Build and test XiX with OCaml 4.14.2 via OPAM on Ubuntu.
# We are testing the bootstrap-mk.sh and mk way to build XiX, not dune!
# See also .github/workflows/docker.yml for its use in Github Actions (GHA).
#
# history:
#  - try to use OCaml 3.10.0 with Alpine 3.21 but clang compilation error
#    requiring to patch ocamlc for -Wno-error=implicit-function-declaration and
#    then missing getwd() old glibc function (Alpine use Musl libc)
#    => simpler to switch to Ubuntu
#  - use 3.10.0 (on Ubuntu) because it was a version we managed to port to plan9?
#    (the byterun/ from ocaml 1.07 managed to run bytecode from 3.10.0?)
#    and oldest version that can work for stdcompat
#  - use 4.09.1 because oldest version that can work with ppx_deriving and
#    stdcompat and Cap.ml
#    (in theory ppx_deriving works for 4.05.0 but Cap.mli does not parse still)
#  - use 4.12.0 because oldest version with stdlib/either.ml
#  - use 4.14.2 because want to get rid of some use of Stdcompat
#    (we could use 4.13.0 but got weird errors in docker so 4.14.2)
#
# Note that even if we requires 4.14.2 here, which is relatively recent, we now
# also check we build with ocaml-light (=~ 1.07 + recent patches) in
# Dockerfile.light! and we also check it builds with OCaml 5.3.0 in shell.nix
# Note that xix uses Stdcompat so xix should compile with many different versions
# of OCaml (including ocaml-light since it defines also a toy stdcompat.ml).
# update: I use a bit less stdcompat, partly for mkconfig.nostdlib,
# so we might break with more older versions of OCaml

FROM ubuntu:22.04
#alt: alpine:3.21, ubuntu:24.04, opam/ocaml (but simpler to remove intermediates)

# Setup a basic C dev environment
RUN apt-get update # needed otherwise can't find any package
RUN apt-get install -y build-essential autoconf automake pkgconf
#alt: apk add build-base make bash git rsync curl
#alt: LATER: use kencc or better goken or better BOOTSTRAP/{7a,7c,7l}
# and compile our own ocaml-light and then xix

# Setup OPAM and OCaml
RUN apt-get install -y opam
#alt: install old OCaml from tar (without opam) and install stdcompat
#alt: RUN apk add opam
RUN opam init --disable-sandboxing -y  # (disable sandboxing due to Docker)
ARG OCAML_VERSION=4.14.2
RUN opam switch create ${OCAML_VERSION} -v

WORKDIR /src

# Install dependencies
# copy enough files for configure below to work
COPY configure xix.opam mkconfig.unix ./
# 9base for rc (TODO: delete once we can bootstrap a working bin/rc)
# zlib for ogit (TODO: delete we should do our own unzip)
RUN apt-get install -y 9base zlib1g-dev
RUN ./configure

# Now let's build from source
COPY . .

# Bootstrap
RUN eval $(opam env) && ./bootstrap-mk.sh

# Full build
#coupling: env.sh
ENV PATH="$PATH:/src/bin"
# TODO: at some point use the boostrapped /src/bin/rc
ENV MKSHELL="/usr/bin/rc"
RUN eval $(opam env) && omk clean
RUN eval $(opam env) && omk depend
RUN eval $(opam env) && omk
RUN eval $(opam env) && omk opt
# alt: use dune but xix spirit to limit external tools and dogfood mk

# Test
RUN ./test.sh
