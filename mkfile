TOP=.
<mkconfig

#TODO: should be a builtin env var defined both in mk and omk to be argv0
MK=omk

# STDLIB is defined or not (usually not) in mkconfig
DIRS=\
  $STDLIB lib_core/commons lib_core/regexps \
  builder shell\
  lib_parsing generators/lex generators/yacc\
  macroprocessor\
  assembler/objects assembler\
  linker/executables linker/libraries linker\
  compiler\
  utilities/files

#TODO: does not work yet with ocaml-light: vcs

# this does not work anymore with OCaml5 (no ThreadUnix) and
# while it may compile for Unix, it can only work when run on Plan9
DIRS_PLAN9=\
  lib_core/system/plan9 \
  lib_graphics/geometry lib_graphics/draw lib_graphics/input lib_graphics/ui \
  windows \
  kernel/core kernel/concurrency_ kernel/base kernel/concurrency \
  kernel/memory kernel/processes kernel/scheduler kernel/time

#TODO: rename to TESTDIRS, reduce to just tests/ and in tests/ recurse
TESTDIRS1=\
  tests/assembler tests/compiler tests/linker

# works only under plan9 for now
TESTDIRS2=windows/tests lib_system/plan9/tests

all:V: all.directories
opt:V: opt.directories
depend:V: depend.directories

%.directories:V:
	for(i in $DIRS) @{
		echo $i/
		cd $i
		$MK $MKFLAGS $stem
	}

# alternate style to the %.directories trick; even simpler
clean nuke:V:
	for(i in $DIRS $DIRS_PLAN9 $TESTDIRS) @{
		cd $i
		$MK $MKFLAGS $target
	}

# those targets require to have run 'mk all' first
plan9.%:V:
	for(i in $DIRS_PLAN9) @{
		echo $i/
		cd $i
		$MK $MKFLAGS $stem
	}

# too many dupes for now (e.g., Ast.ml in mutliple dirs)
graph:QV:
	codegraph_build -symlinks -lang cmt -verbose .
