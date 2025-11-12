
Options new in this version:

-d
	Elide the dynamic linking header.  With this option, the binary
	is statically linked and does not refer to dynld.  Without this option
	(the default), the binary's contents are identical but it is loaded with dynld.
-e
	Emit an extra ELF-compatible symbol table useful with tools such as
	nm, gdb, and oprofile. This option makes the binary file considerably larger.
-H6
	Write Apple Mach-O binaries (default when $GOOS is darwin)
-H7
	Write Linux ELF binaries (default when $GOOS is linux)
-L dir1,dir2,..
	Search for libraries (package files) in the comma-separated list of directories.
	The default is the single location $GOROOT/pkg/$GOOS_amd64.
-r dir1:dir2:...
	Set the dynamic linker search path when using ELF.
-V
	Print the linker version.
