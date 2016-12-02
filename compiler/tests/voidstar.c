int *x;

// 5c allows this (with -V or without)
void *x;

// but not this if you use 5c -V

int foo(int *x);

int foo(void *x);
