void foo() {
  int a[10];
  int b[5];
  int *c;

  float *x;

  //x = b;
  //c = a;
  a = c; // not an l-value!

}
