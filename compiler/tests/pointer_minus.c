void foo() {
  int *x;
  int *y;
  int *z;
  long d;

  z = x + 1;
  z = 1 + x;

  // forbidden
  // x+y; 
  // ok with 5c
  d = x - y;

  // forbidden
  //x+=y; 
  // forbidden
  // x-=y;
  // because convert to
  // x = x - y;
  // and x should be a long!

}
