%token	<lval>	LHI LLO LMREG 
%token	<lval>	LTYPEX LFCREG LM
%token	<lval>	LFCR LSCHED
%type	<gen>	vgen lgen vlgen freg mreg fcreg
%type	<gen>	oreg nireg fgen
%%

line:
|	LSCHED ';'
	{
		nosched = $1;
	}

inst:
/*
 * LOAD/STORE, but not MOVW
 */
|	LTYPE3 lgen ',' gen
	{
		if(!isreg(&$2) && !isreg(&$4))
			print("one side must be register\n");
		outcode($1, &$2, NREG, &$4);
	}
/*
 * SPECIAL
 */
|	LTYPE4 comma
	{
		outcode($1, &nullgen, NREG, &nullgen);
	}

/*
 * MOVW
 */
|	LTYPE5 vlgen ',' vgen
	{
		if(!isreg(&$2) && !isreg(&$4))
			print("one side must be register\n");
		outcode($1, &$2, NREG, &$4);
	}

/*
 * MUL/DIV
 */
|	LTYPE6 reg ',' sreg comma
	{
		outcode($1, &$2, $4, &nullgen);
	}
|	LTYPE6 reg ',' sreg ',' reg
	{
		outcode($1, &$2, $4, &$6);
	}

/*
 * JMP/JAL
 */
|	LTYPE7 comma rel
	{
		outcode($1, &nullgen, NREG, &$3);
	}
|	LTYPE7 comma nireg
	{
		outcode($1, &nullgen, NREG, &$3);
	}

|	LTYPE8 comma rel
	{
		outcode($1, &nullgen, NREG, &$3);
	}
|	LTYPE8 comma nireg
	{
		outcode($1, &nullgen, NREG, &$3);
	}

|	LTYPE8 sreg ',' nireg
	{
		outcode($1, &nullgen, $2, &$4);
	}

/*
 * BEQ/BNE
 */
|	LTYPE9 gen ',' rel
	{
		if(!isreg(&$2))
			print("left side must be register\n");
		outcode($1, &$2, NREG, &$4);
	}
|	LTYPE9 gen ',' sreg ',' rel
	{
		if(!isreg(&$2))
			print("left side must be register\n");
		outcode($1, &$2, $4, &$6);
	}
/*
 * B-other
 */
|	LTYPEA gen ',' rel
	{
		if(!isreg(&$2))
			print("left side must be register\n");
		outcode($1, &$2, NREG, &$4);
	}

/*
 * floating-type
 */
|	LTYPED freg ',' freg
	{
		outcode($1, &$2, NREG, &$4);
	}
|	LTYPEE freg ',' freg
	{
		outcode($1, &$2, NREG, &$4);
	}
|	LTYPEE freg ',' LFREG ',' freg
	{
		outcode($1, &$2, $4, &$6);
	}
|	LTYPEF freg ',' LFREG comma
	{
		outcode($1, &$2, $4, &nullgen);
	}
/*
 * coprocessor branch
 */
|	LTYPEG comma rel
	{
		outcode($1, &nullgen, NREG, &$3);
	}
/*
 * word
 */
|	LTYPEH comma ximm
	{
		outcode($1, &nullgen, NREG, &$3);
	}
/*
 * NOP
 */
|	LTYPEI comma
	{
		outcode($1, &nullgen, NREG, &nullgen);
	}
|	LTYPEI ',' vgen
	{
		outcode($1, &nullgen, NREG, &$3);
	}
|	LTYPEI vgen comma
	{
		outcode($1, &$2, NREG, &nullgen);
	}
/*
 * BREAK -- overloaded with CACHE opcode
 */
|	LTYPEJ comma
	{
		outcode($1, &nullgen, NREG, &nullgen);
	}
|	LTYPEJ vgen ',' vgen
	{
		outcode($1, &$2, NREG, &$4);
	}


vlgen:
	lgen

|	fgen
|	mreg
|	fcreg
|	LHI
	{
		$$ = nullgen;
		$$.type = D_HI;
	}
|	LLO
	{
		$$ = nullgen;
		$$.type = D_LO;
	}

vgen:
	gen

|	fgen
|	mreg
|	fcreg
|	LHI
	{
		$$ = nullgen;
		$$.type = D_HI;
	}
|	LLO
	{
		$$ = nullgen;
		$$.type = D_LO;
	}

lgen:
	gen
|	ximm

fgen:
	freg

mreg:
	LMREG
	{
		$$ = nullgen;
		$$.type = D_MREG;
		$$.reg = $1;
	}
|	LM '(' con ')'
	{
		$$ = nullgen;
		$$.type = D_MREG;
		$$.reg = $3;
	}

fcreg:
	LFCREG
	{
		$$ = nullgen;
		$$.type = D_FCREG;
		$$.reg = $1;
	}
|	LFCR '(' con ')'
	{
		$$ = nullgen;
		$$.type = D_FCREG;
		$$.reg = $3;
	}

freg:
	LFREG
	{
		$$ = nullgen;
		$$.type = D_FREG;
		$$.reg = $1;
	}
|	LF '(' con ')'
	{
		$$ = nullgen;
		$$.type = D_FREG;
		$$.reg = $3;
	}

ximm:	
|	'$' oreg
	{
		$$ = $2;
		$$.type = D_CONST;
	}
|	'$' '*' '$' oreg
	{
		$$ = $4;
		$$.type = D_OCONST;
	}

|	'$' LFCONST
	{
		$$ = nullgen;
		$$.type = D_FCONST;
		$$.dval = $2;
	}
|	'$' '-' LFCONST
	{
		$$ = nullgen;
		$$.type = D_FCONST;
		$$.dval = -$3;
	}

nireg:
	ireg
|	con ireg
	{
		if($1 != 0)
			yyerror("offset must be zero");
		$$ = $2;
	}
|	name
	{
		$$ = $1;
		if($1.name != D_EXTERN && $1.name != D_STATIC) {
		}
	}

gen:
	reg
|	con
	{
		$$ = nullgen;
		$$.type = D_OREG;
		$$.offset = $1;
	}
|	oreg

oreg:
	name
|	name '(' sreg ')'
	{
		$$ = $1;
		$$.type = D_OREG;
		$$.reg = $3;
	}
|	'(' sreg ')'
	{
		$$ = nullgen;
		$$.type = D_OREG;
		$$.reg = $2;
		$$.offset = 0;
	}
|	con '(' sreg ')'
	{
		$$ = nullgen;
		$$.type = D_OREG;
		$$.reg = $3;
		$$.offset = $1;
	}

name:
	con '(' pointer ')'
	{
		$$ = nullgen;
		$$.type = D_OREG;
		$$.name = $3;
		$$.sym = S;
		$$.offset = $1;
	}
