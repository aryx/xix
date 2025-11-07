// this feature is used in the code of ocaml for CAMLparam macros

#define FOO(a) int xx__##a;

int main(void) {
  FOO(i);
  return xx__i;
}
