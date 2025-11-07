int foo(int x) {
  int res;

  // this actually does not generate any branching code on ARM
  // because of the conditional execution trick when optimized!

//	CMP	$0,R0,
//	MOVW.NE	$1,R2
//	MOVW.EQ	$2,R2
//	MOVW	R2,R0
//	RET	,

  if(x) {
    res = 1;
  } else {
    res = 2;
  }
  return res;
}
