int foo() {
  int x;

  x = 0;
  goto L1;
  x = 1;
  goto L1;
  x = 2;
  
L1:
  x = 3;

  return x;
}
