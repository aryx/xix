
// clang issues a warning (-Wextern-initializer), 5c says nothing, hmm
extern int x = 1;

// clang says nothing here, but it should I think.
extern int foo() {
  return 1;
}
