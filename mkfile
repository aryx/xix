# -*- sh -*-
</$objtype/mkfile
<mkconfig

DIRS=\
  lib_core commons\
  mk\
  lib_parsing lex yacc\
  formats/objects assembler\
  formats/executables linker\

all:V: all.directories
opt:V: opt.directories
clean:V: clean.directories
depend:V: depend.directories

%.directories:V:
	for(i in $DIRS) @{
		cd $i
		echo $i/
		mk $MKFLAGS $stem
	}
