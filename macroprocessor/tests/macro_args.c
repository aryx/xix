#define FOO(a,b) a+b

int main() {
  if(1)
    return FOO(1,2);
  else
    return FOO (1,2);
}
