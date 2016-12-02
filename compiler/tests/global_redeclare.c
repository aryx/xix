
int foo;

// we should warn about useless declaration before, or is it for
// forward decl?
int foo = 1;

// 5c does not say anything, hmmm, at least clang warns
int foo = 2;
