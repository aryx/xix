<mkconfig

DIRS1=\
  lib_core/collections lib_core/commons\
  mk shell\
  lib_parsing lex yacc\
  macroprocessor\
  lib_toolchain/objects assembler\
  lib_toolchain/executables linker\
  compiler \
  version_control

# works only under plan9 for now: 
DIRS2=lib_core/commons2 lib_system/plan9 lib_graphics windows

# works only from scratch:
DIRS3=kernel

DIRS=$DIRS1 #$DIRS2 #DIRS3

#TODO: add the other dirs from DIRS1 and remove once DIRS_LIGHT==DIRS1
DIRS_LIGHT=\
  lib_core/collections lib_core/commons\
  mk shell\
  lib_parsing lex yacc\
  macroprocessor \
  lib_toolchain/objects \
  lib_toolchain/executables

#TODO: assembler linker compiler version_control


TESTDIRS=\
  assembler/tests compiler/tests linker/tests \
  windows/tests \
  lib_system/plan9/tests

all:V: all.directories
opt:V: opt.directories
depend:V: depend.directories

%.directories:V:
	for(i in $DIRS) @{
		echo $i/
		cd $i
		mk $MKFLAGS $stem
	}

# alternate style to the %.directories trick; even simpler
clean nuke:V:
	for(i in $DIRS $TESTDIRS) @{
		cd $i
		mk $MKFLAGS $target
	}

# ???
SUBDIRS=$DIRS $TESTDIRS

%.light:V:
	for(i in $DIRS_LIGHT) @{
		echo $i/
		cd $i
		mk $MKFLAGS $stem
	}

# too many dupes for now (e.g., ast.ml in mutliple dirs)
graph:QV:
	codegraph_build -symlinks -lang cmt -verbose .
