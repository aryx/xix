# -*- sh -*-
</$objtype/mkfile
<mkconfig

DIRS=commons\
  mk\
  formats/objects formats/executables\
  assembler linker

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
