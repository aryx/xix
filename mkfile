# -*- sh -*-
</$objtype/mkfile
<mkconfig

DIRS1=\
  lib_core/collections commons\
  mk shell\
  lib_parsing lex yacc\
  macroprocessor\
  formats/objects assembler\
  formats/executables linker\
  compiler

# works only under plan9 for now: 
DIRS2=commons2 lib_system/plan9 lib_graphics windows

# works only from scratch:
DIRS3=kernel

DIRS=$DIRS1 $DIRS2 #DIRS3

all:V: all.directories
opt:V: opt.directories
clean:V: clean.directories
depend:V: depend.directories

%.directories:V:
	for(i in $DIRS) @{
		echo $i/
		cd $i
		mk $MKFLAGS $stem
	}
