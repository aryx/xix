
typedef int foo;

void bar() {
  foo foo;

  {
    //typedef int foo; // forbidden cos not at toplevel for now
    return foo;
  }
  
  return foo;
}
// this would generate an error.
//return foo;
