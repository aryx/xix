%type	<lval>	oexpr creg reglist
%type	<gen>	regreg

inst:
/*
 * BX
 */
|	LTYPEBX comma ireg
	{
		outcode($1, Always, &nullgen, NREG, &$3);
	}
/*
 * MOVM
 */
|	LTYPE8 cond ioreg ',' '[' reglist ']'
	{
		Gen g;

		g = nullgen;
		g.type = D_CONST;
		g.offset = $6;
		outcode($1, $2, &$3, NREG, &g);
	}
|	LTYPE8 cond '[' reglist ']' ',' ioreg
	{
		Gen g;

		g = nullgen;
		g.type = D_CONST;
		g.offset = $4;
		outcode($1, $2, &g, NREG, &$7);
	}
/*
 * CASE
 */
|	LTYPED cond reg comma
	{
		outcode($1, $2, &$3, NREG, &nullgen);
	}

/*
 * floating-point coprocessor
 */
|	LTYPEI cond freg ',' freg
	{
		outcode($1, $2, &$3, NREG, &$5);
	}

/*
 * MCR MRC
 */
|	LTYPEJ cond con ',' expr ',' spreg ',' creg ',' creg oexpr
	{
		Gen g;

		g = nullgen;
		g.type = D_CONST;
		g.offset =
			(0xe << 24) |		/* opcode */
			($1 << 20) |		/* MCR/MRC */
			($2 << 28) |		/* scond */
			(($3 & 15) << 8) |	/* coprocessor number */
			(($5 & 7) << 21) |	/* coprocessor operation */
			(($7 & 15) << 12) |	/* arm register */
			(($9 & 15) << 16) |	/* Crn */
			(($11 & 15) << 0) |	/* Crm */
			(($12 & 7) << 5) |	/* coprocessor information */
			(1<<4);			/* must be set */
		outcode(AWORD, Always, &nullgen, NREG, &g);
	}

/*
 * MULL hi,lo,r1,r2
 */
|	LTYPEM cond reg ',' reg ',' regreg
	{
		outcode($1, $2, &$3, $5.reg, &$7);
	}

/*
 * MULA hi,lo,r1,r2
 */
|	LTYPEN cond reg ',' reg ',' reg ',' spreg 
	{
		$7.type = D_REGREG;
		$7.offset = $9;
		outcode($1, $2, &$3, $5.reg, &$7);
	}


ximm:	
|	'$' '*' '$' oreg
	{
		$$ = $4;
		$$.type = D_OCONST;
	}
|	fcon


reglist:
	spreg
	{
		$$ = 1 << $1;
	}
|	spreg '-' spreg
	{
		int i;
		$$=0;
		for(i=$1; i<=$3; i++)
			$$ |= 1<<i;
		for(i=$3; i<=$1; i++)
			$$ |= 1<<i;
	}
|	spreg comma reglist
	{
		$$ = (1<<$1) | $3;
	}

gen:
|	shift '(' spreg ')'
	{
		$$ = $1;
		$$.reg = $3;
	}
|	LPSR
	{
		$$ = nullgen;
		$$.type = D_PSR;
		$$.reg = $1;
	}
|	LFCR
	{
		$$ = nullgen;
		$$.type = D_FPCR;
		$$.reg = $1;
	}
|	con
	{
		$$ = nullgen;
		$$.type = D_OREG;
		$$.offset = $1;
	}
|	freg


oreg:
|	name '(' sreg ')'
	{
		$$ = $1;
		$$.type = D_OREG;
		$$.reg = $3;
	}


regreg:
	'(' spreg ',' spreg ')'
	{
		$$ = nullgen;
		$$.type = D_REGREG;
		$$.reg = $2;
		$$.offset = $4;
	}

creg:
	LCREG
|	LC '(' expr ')'
	{
		if($3 < 0 || $3 >= NREG)
			print("register value out of range\n");
		$$ = $3;
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

oexpr:
	{
		$$ = 0;
	}
|	',' expr
	{
		$$ = $2;
	}
