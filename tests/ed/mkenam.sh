oed -r - 5.out.h <<'!'
v/^	A/d
1,$s/^	A/	"/
1,$s/, *$/",/
1i
char*	anames[] =
{
.
$a
};
.
w /tmp/enam.c
q
!
