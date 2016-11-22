extern	long	pwrite(int, void*, long, long long);

void main() {
  pwrite(1, "hello world\n", 12, 0);
}
