
int foo;
// useless redeclaration
//int foo;

// this is ok, you give more precision, the previous one could have been
// a forward decl
int foo = 1;

// 5c does not say anything, hmmm, at least clang warns
int foo = 2;
