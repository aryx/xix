
struct Foo x;

// should warn! 5c does not (clang does)
struct Bar y;

struct Foo {
  int x;
  int y;
};
