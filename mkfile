# -*- sh -*-
</$objtype/mkfile
<mkconfig

DIRS=\
  lib_core/collections commons\
  mk shell\
  lib_parsing lex yacc\
  macroprocessor\
  formats/objects assembler\
  formats/executables linker\
  compiler

# works only under plan9 for now: lib_graphics/* windows/ (and commons2/)
# works only from scratch: kernel/

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
