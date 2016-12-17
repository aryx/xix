int x;

int foo() {
  int *y;

  x = 42;

  y = &x;

  *y = 1;

  return *y;

}
