void foo() {
  int *x;

  // switch.c:5 incompatible types: "INT" and "IND INT" for op "SUB"
  // quite cryptic, but because 5c transform this in 
  // switch(0 - (0 - x)), but that way we forbid pointers or
  // struct or whatever as argument to the switch.
  switch(x) {
  }

}
