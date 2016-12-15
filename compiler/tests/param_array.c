void foo(int x[42]) {

}

int main() {
  // 5c and clang says nothing => better forbid state size of array in foo
  int y[43];

  foo(y);
}
