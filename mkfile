TOP=.
<mkconfig

MK=omk

# STDLIB is defined or not (usually not) in mkconfig

DIRS0=\
  $STDLIB lib_core/commons\
  builder shell\
  lib_parsing generators/lex generators/yacc\
  macroprocessor\
  assembler/objects assembler\
  linker/executables linker/libraries linker\
  compiler

# does not work yet with ocaml-light
DIRS1=version_control

# works only under plan9 for now: 
DIRS2=lib_core/commons_plan9 lib_system/plan9 lib_graphics windows

# works only from scratch:
DIRS3=kernel

DIRS=$DIRS0 #$DIRS1 #$DIRS2 #DIRS3

#TODO: add tests/
TESTDIRS=\
  assembler/tests compiler/tests linker/tests

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
	for(i in $DIRS $TESTDIRS) @{
		cd $i
		$MK $MKFLAGS $target
	}

# too many dupes for now (e.g., Ast.ml in mutliple dirs)
graph:QV:
	codegraph_build -symlinks -lang cmt -verbose .
