int *x;
// 5c allows this (with -V or without), because pointer at the top of the type
// clang does not
void *x = (void*)0;

// but not this if you use 5c -V, because pointer is not at the top
// of the type (but inside a TFUNC)
int foo(int *x);

int foo(void *x);
