struct Foo {
  int x;
  int y;
};

struct Bar {
  struct Foo; // anon structure element
  int z;
};

int foo (struct Bar* x) {
  return x->x; // sugar for x->__Foo.x
}
