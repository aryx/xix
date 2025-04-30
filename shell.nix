# Run 'nix-shell --pure' from the root of the project to get a dev environment
# ready to compile/test/run xix from Linux or macOS, on amd64 or arm64.
# See https://nixos.org/, https://shopify.engineering/what-is-nix,
# https://nix.dev/tutorials/first-steps/declarative-shell and
# https://nix.dev/tutorials/nix-language for more info on Nix.
# alt: flake.nix, which handles more things, but is also more complicated

let
   # fetch a specific nixos version for better reproducibility
   # note that 23.11 was failing on my arm64 macOS hence the use of 24.05
   nixpkgs = fetchTarball "https://github.com/NixOS/nixpkgs/tarball/nixos-24.05";
   pkgs = import nixpkgs { config = {}; overlays = []; };
 in

# Note that running commands under this shell will not be isolated; you can
# still modify files outside the repo. In fact, this file is simpler than
# flake.nix because we rely on Opam to install dependencies (in _opamroot/),
# not Nix. This is IMHO a best-of-both world approach: use Nix for portable
# and reproducible external deps and Opam for the familiar OCaml deps.
pkgs.mkShell {
   packages = with pkgs; [

     # OCaml
     opam

     # compile-time external libs
     pcre

     # utilities for opam (cacert is needed by curl)
     git curl cacert
     # optional utilities for development/debugging
     which
     # for some OCaml libs
     autoconf

     # implicit utilities and libs installed by default in nix
     # (see "echo $PATH | sed -e 's/:/\n/g'" and "ldd ./bin/hello-world"):
     # - gnumake bash
     # - binutils gcc/clang glibc linux-headers gnu-config update-autotools
     # - coreutils findutils diffutils file gnugrep gnused
     # - gnutar gzip bzip2 unzip xz brotli zlib zstd curl
     # - not really needed but here: ed gawk patch patchelf
   ];

   shellHook = ''
    # for opam init below
    export OPAMROOT=`pwd`/_opamroot
    if [ ! -d _opamroot ]
    then
       ## --disable-sandboxing because can't find macos sandbox-exec
       ## when running with --pure
       ## --no-depexts because can't find pacman or brew when running
       ## with --pure
       ## -n to answer no to questions such as 'modify ~/.bash_profile?'
       opam init --disable-sandboxing --no-depexts -n

       # Note that the 'opam install' part is now done outside
       # the shellHook in ./configure
       #alt: opam install --deps-only ./hello-world.opam -y
       # but does not work when executed inside a shellHook, weird;
       # I get compilation errors when installing dune
       #alt: opam switch create ./ --deps-only -y
       # to create in a _opam but seems redundant with _opamroot
       # and I also get compilation errors when installing dune
    fi
    eval $(opam env)
  '';
}
