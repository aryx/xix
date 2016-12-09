extern	long	pwrite(int, char*, long, long long);

void main() {
  pwrite(1, "hello world\n", 12, 0);
}
