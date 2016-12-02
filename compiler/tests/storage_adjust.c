extern int x;
int x = 1;

extern int y;
int y;


int z;
extern int z; // should warn

extern int w = 1; // should warn

// always trigger an error message at the toplevel
auto int a;

static int s;
int s;
extern int s;
