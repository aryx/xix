#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <dirent.h>
#include <errno.h>

// for bind
#include <lib9.h>

int main() {
    DIR *dir;
    struct dirent *de;

    if (bind("/tests/bytecode", "/tests/test", MREPL) < 0) {
        fprintf(stderr, "bind failed: %r\n");
        exit(1);
    }

    dir = opendir("/tests/test");
    if (dir == NULL) {
        perror("opendir");
        exit(1);
    }

    while ((de = readdir(dir)) != NULL) {
        printf("%s\n", de->d_name);
    }

    closedir(dir);
    return 0;
}
