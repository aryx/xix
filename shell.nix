{ pkgs ? import <nixpkgs> {} }:

let
  ocamlVersion = "4.09.1";
in
pkgs.mkShell {
  buildInputs = [
    pkgs.ocaml
    pkgs.opam
    pkgs.gnused
    pkgs.gnugrep
  ];

  shellHook = ''
    export OPAMROOT=$PWD/.opam
    export OPAMSWITCH=$PWD/_opam_switch
    export OPAMYES=1
    export OPAMCONFIRMLEVEL=unsafe-yes

    if [ ! -d "$OPAMROOT" ]; then
      echo "[*] Initializing opam..."
      opam init --bare --disable-sandboxing --no-setup
    fi

    eval $(opam env --root=$OPAMROOT --switch=$OPAMSWITCH)

    if ! opam switch list --short | grep -qx "$OPAMSWITCH"; then
      echo "[*] Creating local switch at $OPAMSWITCH..."
      opam switch create "$OPAMSWITCH" "ocaml-base-compiler.${ocamlVersion}"
    fi

    eval $(opam env --root=$OPAMROOT --switch=$OPAMSWITCH)

    if [ -f xix.opam ]; then
      echo "[*] Installing dependencies from xix.opam..."
      opam install . --deps-only
    fi
  '';
}
