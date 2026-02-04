#include <u.h>
#include <libc.h>

void main() {
    int ret, n;
    fdt fd;
    char buf[ERRMAX];
	Dir *d;

    buf[0] = '\0';

    ret = bind("/tests/bytecode", "/tests/test", MREPL);
    if (ret < 0) {
        errstr(buf, sizeof buf);
        print("error: %s\n", buf);
        exits("error");
    }
    // else
    fd = open("/tests/test", OREAD);
	if(fd < 0)
		sysfatal("open: %r");    
    

    while((n = dirread(fd, &d)) > 0){
		for(int i = 0; i < n; i++)
			print("%s\n", d[i].name);
		free(d);
	}

	if(n < 0)
		sysfatal("dirread: %r");

	close(fd);
	exits(nil);
}
