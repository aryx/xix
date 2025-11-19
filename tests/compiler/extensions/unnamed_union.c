struct Foo {
  // anonymous struct and anonymous field!
  struct {
    float z;
  };
  // this is common in regular C code
  union {
    int x;
    float y;
  };
};


struct Bar {
  struct Foo;
};

float foo () {
  struct Foo x;

  return x.z;
}
