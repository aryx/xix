# -*- sh -*-
</$objtype/mkfile
<mkconfig

DIRS=\
  lib_core commons\
  mk shell\
  lib_parsing lex yacc\
  macroprocessor\
  formats/objects assembler\
  formats/executables linker\
  compiler


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
