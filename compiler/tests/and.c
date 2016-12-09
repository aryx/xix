int foo() {
  // short x; this generates an error
  int x;
  // 5c has as special case for int and float. Not sure why.
  return x & 1.3;
}
