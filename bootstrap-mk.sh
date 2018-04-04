#!/bin/sh

# this file was mostly auto-generated by copy-pasting a trace of mk

# any error should abort the script
set -x

# for windows under cygwin
#EXTRALINKFLAGS=-custom

TOP=`pwd`

cd $TOP/lib_core/collections/
ocamlc.opt -g -dtypes -bin-annot -absname  -c set_.mli
ocamlc.opt -g -dtypes -bin-annot -absname  -c map_.mli
ocamlc.opt -g -dtypes -bin-annot -absname  -c set_.ml
ocamlc.opt -g -dtypes -bin-annot -absname  -c map_.ml
ocamlc.opt  set_.cmo map_.cmo -a -o lib.cma

cd $TOP/lib_core/commons/
ocamlc.opt -g -dtypes -bin-annot -absname -I . -c common.mli
ocamlc.opt -g -dtypes -bin-annot -absname -I . -c common2.ml
ocamlc.opt -g -dtypes -bin-annot -absname -I . -c IO.mli
ocamlc.opt -g -dtypes -bin-annot -absname -I . -c date.mli
ocamlc.opt -g -dtypes -bin-annot -absname -I . -c common.ml
ocamlc.opt -g -dtypes -bin-annot -absname -I . -c ocaml.mli
ocamlc.opt -g -dtypes -bin-annot -absname -I . -c date.ml
ocamlc.opt -g -dtypes -bin-annot -absname -I . -c IO.ml
ocamlc.opt -g -dtypes -bin-annot -absname -I . -c ocaml.ml
ocamlc.opt -I . common.cmo common2.cmo ocaml.cmo IO.cmo date.cmo -a -o lib.cma

cd $TOP/mk
ocamlyacc parser.mly
ocamllex lexer.mll
ocamlc.opt -g -dtypes -bin-annot -absname -I ../lib_core/commons -I ../lib_core/collections -c globals.ml
ocamlc.opt -g -dtypes -bin-annot -absname -I ../lib_core/commons -I ../lib_core/collections -c flags.ml
ocamlc.opt -g -dtypes -bin-annot -absname -I ../lib_core/commons -I ../lib_core/collections -c ast.ml
ocamlc.opt -g -dtypes -bin-annot -absname -I ../lib_core/commons -I ../lib_core/collections -c shellenv.mli
ocamlc.opt -g -dtypes -bin-annot -absname -I ../lib_core/commons -I ../lib_core/collections -c percent.mli
ocamlc.opt -g -dtypes -bin-annot -absname -I ../lib_core/commons -I ../lib_core/collections -c file.mli
ocamlc.opt -g -dtypes -bin-annot -absname -I ../lib_core/commons -I ../lib_core/collections -c shellenv.ml
ocamlc.opt -g -dtypes -bin-annot -absname -I ../lib_core/commons -I ../lib_core/collections -c shell.mli
ocamlc.opt -g -dtypes -bin-annot -absname -I ../lib_core/commons -I ../lib_core/collections -c env.mli
ocamlc.opt -g -dtypes -bin-annot -absname -I ../lib_core/commons -I ../lib_core/collections -c parser.mli
ocamlc.opt -g -dtypes -bin-annot -absname -I ../lib_core/commons -I ../lib_core/collections -c parse.mli
ocamlc.opt -g -dtypes -bin-annot -absname -I ../lib_core/commons -I ../lib_core/collections -c percent.ml
ocamlc.opt -g -dtypes -bin-annot -absname -I ../lib_core/commons -I ../lib_core/collections -c rules.ml
ocamlc.opt -g -dtypes -bin-annot -absname -I ../lib_core/commons -I ../lib_core/collections -c file.ml
ocamlc.opt -g -dtypes -bin-annot -absname -I ../lib_core/commons -I ../lib_core/collections -c shell.ml
ocamlc.opt -g -dtypes -bin-annot -absname -I ../lib_core/commons -I ../lib_core/collections -c env.ml
ocamlc.opt -g -dtypes -bin-annot -absname -I ../lib_core/commons -I ../lib_core/collections -c parser.ml
ocamlc.opt -g -dtypes -bin-annot -absname -I ../lib_core/commons -I ../lib_core/collections -c lexer.ml
ocamlc.opt -g -dtypes -bin-annot -absname -I ../lib_core/commons -I ../lib_core/collections -c eval.mli
ocamlc.opt -g -dtypes -bin-annot -absname -I ../lib_core/commons -I ../lib_core/collections -c graph.mli
ocamlc.opt -g -dtypes -bin-annot -absname -I ../lib_core/commons -I ../lib_core/collections -c parse.ml
ocamlc.opt -g -dtypes -bin-annot -absname -I ../lib_core/commons -I ../lib_core/collections -c eval.ml
ocamlc.opt -g -dtypes -bin-annot -absname -I ../lib_core/commons -I ../lib_core/collections -c graph.ml
ocamlc.opt -g -dtypes -bin-annot -absname -I ../lib_core/commons -I ../lib_core/collections -c job.ml
ocamlc.opt -g -dtypes -bin-annot -absname -I ../lib_core/commons -I ../lib_core/collections -c outofdate.mli
ocamlc.opt -g -dtypes -bin-annot -absname -I ../lib_core/commons -I ../lib_core/collections -c scheduler.mli
ocamlc.opt -g -dtypes -bin-annot -absname -I ../lib_core/commons -I ../lib_core/collections -c scheduler.ml
ocamlc.opt -g -dtypes -bin-annot -absname -I ../lib_core/commons -I ../lib_core/collections -c outofdate.ml
ocamlc.opt -g -dtypes -bin-annot -absname -I ../lib_core/commons -I ../lib_core/collections -c main.ml
ocamlc.opt -g $EXTRALINKFLAGS -I ../lib_core/commons -I ../lib_core/collections str.cma unix.cma ../lib_core/collections/lib.cma ../lib_core/commons/lib.cma globals.cmo flags.cmo ast.cmo parser.cmo lexer.cmo parse.cmo shellenv.cmo shell.cmo percent.cmo env.cmo rules.cmo eval.cmo file.cmo graph.cmo job.cmo scheduler.cmo outofdate.cmo main.cmo ../lib_core/collections/lib.cma ../lib_core/commons/lib.cma -o mk

cd $TOP/shell/
ocamlyacc parser.mly
ocamllex lexer.mll
ocamlc.opt -g -dtypes -bin-annot -absname -I ../lib_core/commons -I ../lib_core/collections -c flags.ml
ocamlc.opt -g -dtypes -bin-annot -absname -I ../lib_core/commons -I ../lib_core/collections -c globals.ml
ocamlc.opt -g -dtypes -bin-annot -absname -I ../lib_core/commons -I ../lib_core/collections -c ast.ml
ocamlc.opt -g -dtypes -bin-annot -absname -I ../lib_core/commons -I ../lib_core/collections -c opcode.ml
ocamlc.opt -g -dtypes -bin-annot -absname -I ../lib_core/commons -I ../lib_core/collections -c pattern.mli
ocamlc.opt -g -dtypes -bin-annot -absname -I ../lib_core/commons -I ../lib_core/collections -c prompt.mli
ocamlc.opt -g -dtypes -bin-annot -absname -I ../lib_core/commons -I ../lib_core/collections -c status.mli
ocamlc.opt -g -dtypes -bin-annot -absname -I ../lib_core/commons -I ../lib_core/collections -c path.mli
ocamlc.opt -g -dtypes -bin-annot -absname -I ../lib_core/commons -I ../lib_core/collections -c process.mli
ocamlc.opt -g -dtypes -bin-annot -absname -I ../lib_core/commons -I ../lib_core/collections -c error.mli
ocamlc.opt -g -dtypes -bin-annot -absname -I ../lib_core/commons -I ../lib_core/collections -c builtin.mli
ocamlc.opt -g -dtypes -bin-annot -absname -I ../lib_core/commons -I ../lib_core/collections -c meta_ast.ml
ocamlc.opt -g -dtypes -bin-annot -absname -I ../lib_core/commons -I ../lib_core/collections -c parser.mli
ocamlc.opt -g -dtypes -bin-annot -absname -I ../lib_core/commons -I ../lib_core/collections -c parse.mli
ocamlc.opt -g -dtypes -bin-annot -absname -I ../lib_core/commons -I ../lib_core/collections -c meta_opcode.ml
ocamlc.opt -g -dtypes -bin-annot -absname -I ../lib_core/commons -I ../lib_core/collections -c dumper.mli
ocamlc.opt -g -dtypes -bin-annot -absname -I ../lib_core/commons -I ../lib_core/collections -c compile.mli
ocamlc.opt -g -dtypes -bin-annot -absname -I ../lib_core/commons -I ../lib_core/collections -c runtime.ml
ocamlc.opt -g -dtypes -bin-annot -absname -I ../lib_core/commons -I ../lib_core/collections -c interpreter.mli
ocamlc.opt -g -dtypes -bin-annot -absname -I ../lib_core/commons -I ../lib_core/collections -c pattern.ml
ocamlc.opt -g -dtypes -bin-annot -absname -I ../lib_core/commons -I ../lib_core/collections -c parser.ml
ocamlc.opt -g -dtypes -bin-annot -absname -I ../lib_core/commons -I ../lib_core/collections -c dumper.ml
ocamlc.opt -g -dtypes -bin-annot -absname -I ../lib_core/commons -I ../lib_core/collections -c compile.ml
ocamlc.opt -g -dtypes -bin-annot -absname -I ../lib_core/commons -I ../lib_core/collections -c fn.mli
ocamlc.opt -g -dtypes -bin-annot -absname -I ../lib_core/commons -I ../lib_core/collections -c var.mli
ocamlc.opt -g -dtypes -bin-annot -absname -I ../lib_core/commons -I ../lib_core/collections -c process.ml
ocamlc.opt -g -dtypes -bin-annot -absname -I ../lib_core/commons -I ../lib_core/collections -c error.ml
ocamlc.opt -g -dtypes -bin-annot -absname -I ../lib_core/commons -I ../lib_core/collections -c lexer.ml
ocamlc.opt -g -dtypes -bin-annot -absname -I ../lib_core/commons -I ../lib_core/collections -c op_process.ml
ocamlc.opt -g -dtypes -bin-annot -absname -I ../lib_core/commons -I ../lib_core/collections -c fn.ml
ocamlc.opt -g -dtypes -bin-annot -absname -I ../lib_core/commons -I ../lib_core/collections -c var.ml
ocamlc.opt -g -dtypes -bin-annot -absname -I ../lib_core/commons -I ../lib_core/collections -c prompt.ml
ocamlc.opt -g -dtypes -bin-annot -absname -I ../lib_core/commons -I ../lib_core/collections -c status.ml
ocamlc.opt -g -dtypes -bin-annot -absname -I ../lib_core/commons -I ../lib_core/collections -c path.ml
ocamlc.opt -g -dtypes -bin-annot -absname -I ../lib_core/commons -I ../lib_core/collections -c builtin.ml
ocamlc.opt -g -dtypes -bin-annot -absname -I ../lib_core/commons -I ../lib_core/collections -c op_repl.ml
ocamlc.opt -g -dtypes -bin-annot -absname -I ../lib_core/commons -I ../lib_core/collections -c main.ml
ocamlc.opt -g -dtypes -bin-annot -absname -I ../lib_core/commons -I ../lib_core/collections -c parse.ml
ocamlc.opt -g -dtypes -bin-annot -absname -I ../lib_core/commons -I ../lib_core/collections -c interpreter.ml
ocamlc.opt -g $EXTRALINKFLAGS -I ../lib_core/commons -I ../lib_core/collections str.cma unix.cma ../lib_core/collections/lib.cma ../lib_core/commons/lib.cma flags.cmo globals.cmo ast.cmo meta_ast.cmo opcode.cmo meta_opcode.cmo dumper.cmo compile.cmo runtime.cmo pattern.cmo fn.cmo var.cmo prompt.cmo status.cmo path.cmo process.cmo error.cmo parser.cmo lexer.cmo parse.cmo builtin.cmo op_repl.cmo op_process.cmo interpreter.cmo main.cmo ../lib_core/collections/lib.cma ../lib_core/commons/lib.cma -o rc

cd $TOP
cp mk/mk shell/rc BOOTSTRAP/
echo 'Copy mk/mk somewhere in your PATH. It assumes there is a /usr/bin/rc.'