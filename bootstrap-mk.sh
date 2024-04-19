#!/bin/sh
# Script to compile 'mk' (without using 'mk') and generate a bin/mk
# so that we don't need a BOOTSTRAP/Linux/386/bin/mk like in kencc.
#
# Note that right now to boostrap Xix we still need:
#  - OCaml (which itself requires to bootstrap ocamllex, ocamlyacc and C)
#  - a C compiler
#  - /bin/sh
#
# This file was mostly auto-generated by copy-pasting a trace of mk.

# any error should abort the script
set -e
# for showing the executed commands (verbose)
set -x

#TODO? -bin-annot -absname -dtypes -g
OCAMLCFLAGS=

#TODO? for windows under cygwin might need -custom
EXTRALINKFLAGS=

TOP=`pwd`

cd $TOP/lib_core/collections/
ocamlc.opt $OCAMLCFLAGS  -c set_.mli
ocamlc.opt $OCAMLCFLAGS  -c map_.mli
ocamlc.opt $OCAMLCFLAGS  -c set_.ml
ocamlc.opt $OCAMLCFLAGS  -c map_.ml
ocamlc.opt  set_.cmo map_.cmo -a -o lib.cma

cd $TOP/lib_core/commons/
ocamlc.opt $OCAMLCFLAGS -I . -c common.mli
ocamlc.opt $OCAMLCFLAGS -I . -c common2.ml
ocamlc.opt $OCAMLCFLAGS -I . -c IO.mli
ocamlc.opt $OCAMLCFLAGS -I . -c date.mli
ocamlc.opt $OCAMLCFLAGS -I . -c common.ml
ocamlc.opt $OCAMLCFLAGS -I . -c ocaml.mli
ocamlc.opt $OCAMLCFLAGS -I . -c date.ml
ocamlc.opt $OCAMLCFLAGS -I . -c IO.ml
ocamlc.opt $OCAMLCFLAGS -I . -c ocaml.ml
ocamlc.opt -I . common.cmo common2.cmo ocaml.cmo IO.cmo date.cmo -a -o lib.cma

cd $TOP/mk
ocamlyacc parser.mly
ocamllex lexer.mll
ocamlc.opt $OCAMLCFLAGS -I ../lib_core/commons -I ../lib_core/collections -c globals.ml
ocamlc.opt $OCAMLCFLAGS -I ../lib_core/commons -I ../lib_core/collections -c flags.ml
ocamlc.opt $OCAMLCFLAGS -I ../lib_core/commons -I ../lib_core/collections -c ast.ml
ocamlc.opt $OCAMLCFLAGS -I ../lib_core/commons -I ../lib_core/collections -c shellenv.mli
ocamlc.opt $OCAMLCFLAGS -I ../lib_core/commons -I ../lib_core/collections -c percent.mli
ocamlc.opt $OCAMLCFLAGS -I ../lib_core/commons -I ../lib_core/collections -c file.mli
ocamlc.opt $OCAMLCFLAGS -I ../lib_core/commons -I ../lib_core/collections -c shellenv.ml
ocamlc.opt $OCAMLCFLAGS -I ../lib_core/commons -I ../lib_core/collections -c shell.mli
ocamlc.opt $OCAMLCFLAGS -I ../lib_core/commons -I ../lib_core/collections -c env.mli
ocamlc.opt $OCAMLCFLAGS -I ../lib_core/commons -I ../lib_core/collections -c parser.mli
ocamlc.opt $OCAMLCFLAGS -I ../lib_core/commons -I ../lib_core/collections -c parse.mli
ocamlc.opt $OCAMLCFLAGS -I ../lib_core/commons -I ../lib_core/collections -c percent.ml
ocamlc.opt $OCAMLCFLAGS -I ../lib_core/commons -I ../lib_core/collections -c rules.ml
ocamlc.opt $OCAMLCFLAGS -I ../lib_core/commons -I ../lib_core/collections -c file.ml
ocamlc.opt $OCAMLCFLAGS -I ../lib_core/commons -I ../lib_core/collections -c shell.ml
ocamlc.opt $OCAMLCFLAGS -I ../lib_core/commons -I ../lib_core/collections -c env.ml
ocamlc.opt $OCAMLCFLAGS -I ../lib_core/commons -I ../lib_core/collections -c parser.ml
ocamlc.opt $OCAMLCFLAGS -I ../lib_core/commons -I ../lib_core/collections -c lexer.ml
ocamlc.opt $OCAMLCFLAGS -I ../lib_core/commons -I ../lib_core/collections -c eval.mli
ocamlc.opt $OCAMLCFLAGS -I ../lib_core/commons -I ../lib_core/collections -c graph.mli
ocamlc.opt $OCAMLCFLAGS -I ../lib_core/commons -I ../lib_core/collections -c parse.ml
ocamlc.opt $OCAMLCFLAGS -I ../lib_core/commons -I ../lib_core/collections -c eval.ml
ocamlc.opt $OCAMLCFLAGS -I ../lib_core/commons -I ../lib_core/collections -c graph.ml
ocamlc.opt $OCAMLCFLAGS -I ../lib_core/commons -I ../lib_core/collections -c job.ml
ocamlc.opt $OCAMLCFLAGS -I ../lib_core/commons -I ../lib_core/collections -c outofdate.mli
ocamlc.opt $OCAMLCFLAGS -I ../lib_core/commons -I ../lib_core/collections -c scheduler.mli
ocamlc.opt $OCAMLCFLAGS -I ../lib_core/commons -I ../lib_core/collections -c scheduler.ml
ocamlc.opt $OCAMLCFLAGS -I ../lib_core/commons -I ../lib_core/collections -c outofdate.ml
ocamlc.opt $OCAMLCFLAGS -I ../lib_core/commons -I ../lib_core/collections -c main.ml
ocamlc.opt -g $EXTRALINKFLAGS -I ../lib_core/commons -I ../lib_core/collections str.cma unix.cma ../lib_core/collections/lib.cma ../lib_core/commons/lib.cma globals.cmo flags.cmo ast.cmo parser.cmo lexer.cmo parse.cmo shellenv.cmo shell.cmo percent.cmo env.cmo rules.cmo eval.cmo file.cmo graph.cmo job.cmo scheduler.cmo outofdate.cmo main.cmo ../lib_core/collections/lib.cma ../lib_core/commons/lib.cma -o mk

cd $TOP/shell/
ocamlyacc parser.mly
ocamllex lexer.mll
ocamlc.opt $OCAMLCFLAGS -I ../lib_core/commons -I ../lib_core/collections -c flags.ml
ocamlc.opt $OCAMLCFLAGS -I ../lib_core/commons -I ../lib_core/collections -c globals.ml
ocamlc.opt $OCAMLCFLAGS -I ../lib_core/commons -I ../lib_core/collections -c ast.ml
ocamlc.opt $OCAMLCFLAGS -I ../lib_core/commons -I ../lib_core/collections -c opcode.ml
ocamlc.opt $OCAMLCFLAGS -I ../lib_core/commons -I ../lib_core/collections -c pattern.mli
ocamlc.opt $OCAMLCFLAGS -I ../lib_core/commons -I ../lib_core/collections -c prompt.mli
ocamlc.opt $OCAMLCFLAGS -I ../lib_core/commons -I ../lib_core/collections -c status.mli
ocamlc.opt $OCAMLCFLAGS -I ../lib_core/commons -I ../lib_core/collections -c path.mli
ocamlc.opt $OCAMLCFLAGS -I ../lib_core/commons -I ../lib_core/collections -c process.mli
ocamlc.opt $OCAMLCFLAGS -I ../lib_core/commons -I ../lib_core/collections -c error.mli
ocamlc.opt $OCAMLCFLAGS -I ../lib_core/commons -I ../lib_core/collections -c builtin.mli
ocamlc.opt $OCAMLCFLAGS -I ../lib_core/commons -I ../lib_core/collections -c meta_ast.ml
ocamlc.opt $OCAMLCFLAGS -I ../lib_core/commons -I ../lib_core/collections -c parser.mli
ocamlc.opt $OCAMLCFLAGS -I ../lib_core/commons -I ../lib_core/collections -c parse.mli
ocamlc.opt $OCAMLCFLAGS -I ../lib_core/commons -I ../lib_core/collections -c meta_opcode.ml
ocamlc.opt $OCAMLCFLAGS -I ../lib_core/commons -I ../lib_core/collections -c dumper.mli
ocamlc.opt $OCAMLCFLAGS -I ../lib_core/commons -I ../lib_core/collections -c compile.mli
ocamlc.opt $OCAMLCFLAGS -I ../lib_core/commons -I ../lib_core/collections -c runtime.ml
ocamlc.opt $OCAMLCFLAGS -I ../lib_core/commons -I ../lib_core/collections -c interpreter.mli
ocamlc.opt $OCAMLCFLAGS -I ../lib_core/commons -I ../lib_core/collections -c pattern.ml
ocamlc.opt $OCAMLCFLAGS -I ../lib_core/commons -I ../lib_core/collections -c parser.ml
ocamlc.opt $OCAMLCFLAGS -I ../lib_core/commons -I ../lib_core/collections -c dumper.ml
ocamlc.opt $OCAMLCFLAGS -I ../lib_core/commons -I ../lib_core/collections -c compile.ml
ocamlc.opt $OCAMLCFLAGS -I ../lib_core/commons -I ../lib_core/collections -c fn.mli
ocamlc.opt $OCAMLCFLAGS -I ../lib_core/commons -I ../lib_core/collections -c var.mli
ocamlc.opt $OCAMLCFLAGS -I ../lib_core/commons -I ../lib_core/collections -c process.ml
ocamlc.opt $OCAMLCFLAGS -I ../lib_core/commons -I ../lib_core/collections -c error.ml
ocamlc.opt $OCAMLCFLAGS -I ../lib_core/commons -I ../lib_core/collections -c lexer.ml
ocamlc.opt $OCAMLCFLAGS -I ../lib_core/commons -I ../lib_core/collections -c op_process.ml
ocamlc.opt $OCAMLCFLAGS -I ../lib_core/commons -I ../lib_core/collections -c fn.ml
ocamlc.opt $OCAMLCFLAGS -I ../lib_core/commons -I ../lib_core/collections -c var.ml
ocamlc.opt $OCAMLCFLAGS -I ../lib_core/commons -I ../lib_core/collections -c prompt.ml
ocamlc.opt $OCAMLCFLAGS -I ../lib_core/commons -I ../lib_core/collections -c status.ml
ocamlc.opt $OCAMLCFLAGS -I ../lib_core/commons -I ../lib_core/collections -c path.ml
ocamlc.opt $OCAMLCFLAGS -I ../lib_core/commons -I ../lib_core/collections -c builtin.ml
ocamlc.opt $OCAMLCFLAGS -I ../lib_core/commons -I ../lib_core/collections -c op_repl.ml
ocamlc.opt $OCAMLCFLAGS -I ../lib_core/commons -I ../lib_core/collections -c main.ml
ocamlc.opt $OCAMLCFLAGS -I ../lib_core/commons -I ../lib_core/collections -c parse.ml
ocamlc.opt $OCAMLCFLAGS -I ../lib_core/commons -I ../lib_core/collections -c interpreter.ml
ocamlc.opt -g $EXTRALINKFLAGS -I ../lib_core/commons -I ../lib_core/collections str.cma unix.cma ../lib_core/collections/lib.cma ../lib_core/commons/lib.cma flags.cmo globals.cmo ast.cmo meta_ast.cmo opcode.cmo meta_opcode.cmo dumper.cmo compile.cmo runtime.cmo pattern.cmo fn.cmo var.cmo prompt.cmo status.cmo path.cmo process.cmo error.cmo parser.cmo lexer.cmo parse.cmo builtin.cmo op_repl.cmo op_process.cmo interpreter.cmo main.cmo ../lib_core/collections/lib.cma ../lib_core/commons/lib.cma -o rc

cd $TOP
cp mk/mk shell/rc bin/
echo 'Copy bin/mk bin/rc somewhere in your PATH and sets MKSHELL to point to rc.'
