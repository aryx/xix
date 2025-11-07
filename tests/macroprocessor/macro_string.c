#define foo(a) stuff(a, "a ab" "a")

int stuff(int x, char* s);

void main() {
  return foo(42);
}
