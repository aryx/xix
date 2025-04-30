# Build and test XiX with OCaml 4.09.1 via opam on Ubuntu Linux

# history:
#  - try to use OCaml 3.10.0 with Alpine 3.21 but clang compilation error
#    requiring to patch ocamlc for -Wno-error=implicit-function-declaration and
#    then missing getwd() old glibc function (alpine use musl libc)
#    => simpler to switch to Ubuntu
#  - use 3.10.0 (on Ubuntu) because it was a version we managed to port to plan9?
#    (the byterun/ from ocaml 1.07 managed to run bytecode from 3.10.0?)
#    and oldest version that can work for stdcompat
#  - use 4.09.1 because oldest version that can work with ppx_deriving and
#    stdcompat and Cap.ml
#    (in theory ppx_deriving works for 4.05.0 but Cap.mli does not parse still)

FROM ubuntu:22.04
#alt: alpine:3.21
#alt: opam base image (but simpler to remove unnecessary intermediates)

# Setup a basic C dev environment
RUN apt-get update # needed otherwise can't find any package
RUN apt-get install -y build-essential autoconf automake
#alt: apk add build-base make bash git rsync curl
#alt: LATER: use kencc and compile our own ocaml-light

# Setup OPAM and OCaml
RUN apt-get install -y opam
#alt: install old OCaml from tar (without opam) and install stdcompat
#alt: RUN apk add opam
# Initialize opam (disable sandboxing due to Docker)
RUN opam init --disable-sandboxing -y
RUN opam switch create 4.09.1 -v

WORKDIR /src

# Install dependencies
COPY xix.opam ./
RUN opam install --deps-only -y .
# 9base for rc (TODO: delete once we can bootstrap a working bin/rc)
# zlib for ogit (TODO: delete we should do our own unzip)
RUN apt-get install -y 9base zlib1g-dev

# Now let's build from source
COPY . .

# Bootstrap
RUN eval $(opam env) && ./bootstrap-mk.sh

# Full build
#coupling: env.sh
ENV PATH="$PATH:/src/bin"
# TODO: at some point use the boostrapped /src/bin/rc
ENV MKSHELL="/usr/bin/rc"
RUN eval $(opam env) && mk clean
RUN eval $(opam env) && mk depend
RUN eval $(opam env) && mk
# alt: use dune but xix spirit to limit external tools and dogfood mk

# Test
RUN ./test.sh
