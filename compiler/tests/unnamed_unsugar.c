struct Foo {
  int x;
  int y;
};


struct Bar {
  struct Foo __1;
  int z;
};

int foo(struct Bar* x) {
  return x->__1.x;
}

int bar(struct Foo* x) {
  return x->x;
}


void test_bar() {
  struct Bar x;
  //bar(&x);
  bar(& (&x)->__1);

}
