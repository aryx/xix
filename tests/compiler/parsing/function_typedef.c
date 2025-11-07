typedef int foo(int x);

foo bar;

// 5c allows that because resolve type during parsing, clang does not.
// but can you access the parameter in the body??
foo bar {
}
