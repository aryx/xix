int X;

enum foo {
  X = 1,
};


int y = X;

void foo() {
  enum foo {
    X = 1,
    };

  int y = X;
}
