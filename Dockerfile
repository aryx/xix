# Build XiX with OCaml 3.10.0 (and opam) on Ubuntu Linux

# history:
#  - try to build with Alpine 3.21 but clang compilation error
#    requiring to patch for -Wno-error=implicit-function-declaration and
#    then missing getwd() old glibc function (alpine use musl libc)
#    so simpler to switch to Ubuntu

FROM ubuntu:22.04
#alt: alpine:3.21

# Setup a basic C dev environment
RUN apt-get update && apt-get install -y build-essential autoconf automake
#alt: apk add build-base make bash git rsync curl

# Setup OPAM and OCaml
RUN apt-get install -y opam
#alt: RUN apk add opam
# Initialize opam (disable sandboxing due to Docker)
RUN opam init --disable-sandboxing -y
# We use 3.10.0 because it's a version we managed to port to plan9 (byterun/) 
RUN opam switch create 3.10.0 -v

# Install dependencies
WORKDIR /src
COPY xix.opam ./
RUN opam install --deps-only -y .

# 9base for rc (TODO: delete once we can bootstrap a working bin/rc)
# zlib for ogit
RUN apt-get install -y 9base zlib1g-dev

# Now let's build from source
COPY . .

# Bootstrap
RUN eval $(opam env) && ./bootstrap-mk.sh

# Full build
ENV PATH="$PATH:/src/bin"
ENV MKSHELL="/usr/bin/rc"
RUN eval $(opam env) && mk clean
RUN eval $(opam env) && mk

# Test
RUN ./test.sh
