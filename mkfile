# -*- sh -*-
</$objtype/mkfile
<mkconfig

DIRS=commons formats/objects

all:V: all.directories

clean:V: clean.directories

depend:V: depend.directories

%.directories:V:
	for(i in $DIRS) @{
		cd $i
		echo $i
		mk $MKFLAGS $stem
	}
