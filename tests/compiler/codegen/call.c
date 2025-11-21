int foo(int i) {
    int y;
    y = 2;
    return i + y;
}

int bar() {
    int x;
    x = 0;
    return foo(x);
}
