void foo() {
  // 5c does not parse this, weird, and clang does not warn.
  for (int i;;) {
  }
}
