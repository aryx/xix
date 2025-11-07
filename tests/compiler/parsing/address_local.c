int** foo() {
  int *x;

  //&(&x);

  // 5a and ast_asm5.ml didnt to get the address of
  // a local or parameter originally, but you need this
  // feature!
  return &x;
}
