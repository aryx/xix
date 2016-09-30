char* str = "foo" "bar";

//char* mix = L"foo" "bar";

void main() {
  unsigned int *mix;
  char *x;
  mix = L"foo";

  x = "foo" "bar";
  mix = L"foo" L"bar";
}
