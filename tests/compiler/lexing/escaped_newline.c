// Can you have escaped newline in string inside macro?
#define FOO a\
            + b

char *str = "foo\
bcd";

char c = '\
a';

void main() {

}
