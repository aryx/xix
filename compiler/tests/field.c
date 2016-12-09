struct Foo {
  int x;
  int y;
};

int foo() {
  struct Foo foo;

  return foo.z; // undefined field
}

