struct Foo {
  int x;
  int y;
};

struct Bar {
  int x;
  float z; // 5c accepts even if different names, but not if different type!
};


// clang does not support it, even if structures are the same
struct Foo x;
struct Bar x;

// this is ok with clang
int y;
int y;
