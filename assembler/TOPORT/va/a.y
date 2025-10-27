%token	<lval>	LHI LLO LMREG LTYPEX LFCREG LM	LFCR LSCHED
%type	<gen>	vgen vlgen freg mreg fcreg	fgen

line:
|	LSCHED ';'
	{
		nosched = $1;
	}

inst:
/*
 * coprocessor branch
 */
|	LTYPEG comma rel
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


nireg:
|	con ireg
	{
		if($1 != 0)
			yyerror("offset must be zero");
		$$ = $2;
	}

gen:
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
