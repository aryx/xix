%type	<lval>	con expr oexpr pointer offset sreg spreg creg
%type	<lval>	rcon cond reglist
%type	<gen>	gen rel reg regreg freg shift fcon frcon
%type	<gen>	imm ximm name oreg ireg nireg ioreg imsr
%%

line:
|	LNAME '=' expr ';'
	{
		$1->type = LVAR;
		$1->value = $3;
	}
|	LVAR '=' expr ';'
	{
		if($1->value != $3)
			yyerror("redeclaration of %s", $1->name);
		$1->value = $3;
	}
|	error ';'

inst:
/*
 * ADD
 */
	LTYPE1 cond imsr ',' spreg ',' reg
	{
		outcode($1, $2, &$3, $5, &$7);
	}
|	LTYPE1 cond imsr ',' spreg ','
	{
		outcode($1, $2, &$3, $5, &nullgen);
	}
/*
 * B/BL
 */
|	LTYPE4 cond comma rel
	{
		outcode($1, $2, &nullgen, NREG, &$4);
	}
|	LTYPE4 cond comma nireg
	{
		outcode($1, $2, &nullgen, NREG, &$4);
	}
/*
 * BX
 */
|	LTYPEBX comma ireg
	{
		outcode($1, Always, &nullgen, NREG, &$3);
	}
/*
 * SWI
 */
|	LTYPE6 cond comma gen
	{
		outcode($1, $2, &nullgen, NREG, &$4);
	}
/*
 * CMP
 */
|	LTYPE7 cond imsr ',' spreg comma
	{
		outcode($1, $2, &$3, $5, &nullgen);
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
 * word
 */
|	LTYPEH comma ximm
	{
		outcode($1, Always, &nullgen, NREG, &$3);
	}

/*
 * floating-point coprocessor
 */
|	LTYPEI cond freg ',' freg
	{
		outcode($1, $2, &$3, NREG, &$5);
	}
|	LTYPEK cond frcon ',' freg
	{
		outcode($1, $2, &$3, NREG, &$5);
	}
|	LTYPEK cond frcon ',' LFREG ',' freg
	{
		outcode($1, $2, &$3, $5, &$7);
	}
|	LTYPEL cond freg ',' freg comma
	{
		outcode($1, $2, &$3, $5.reg, &nullgen);
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
/*
 * END
 */
|	LTYPEE comma
	{
		outcode($1, Always, &nullgen, NREG, &nullgen);
	}





cond:
|	cond LCOND
	{
		$$ = ($1 & ~C_SCOND) | $2;
	}
|	cond LS
	{
		$$ = $1 | $2;
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
|	fcon


fcon:
	'$' LFCONST
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
|	oreg
|	freg

nireg:
	ireg
|	name
	{
		$$ = $1;
		if($1.name != D_EXTERN && $1.name != D_STATIC) {
		}
	}

ireg:
	'(' spreg ')'
	{
		$$ = nullgen;
		$$.type = D_OREG;
		$$.reg = $2;
		$$.offset = 0;
	}

ioreg:
	ireg
|	con '(' sreg ')'
	{
		$$ = nullgen;
		$$.type = D_OREG;
		$$.reg = $3;
		$$.offset = $1;
	}

oreg:
	name
|	name '(' sreg ')'
	{
		$$ = $1;
		$$.type = D_OREG;
		$$.reg = $3;
	}
|	ioreg

reg:
	spreg
	{
		$$ = nullgen;
		$$.type = D_REG;
		$$.reg = $1;
	}

regreg:
	'(' spreg ',' spreg ')'
	{
		$$ = nullgen;
		$$.type = D_REGREG;
		$$.reg = $2;
		$$.offset = $4;
	}

shift:
	spreg '<' '<' rcon
	{
		$$ = nullgen;
		$$.type = D_SHIFT;
		$$.offset = $1 | $4 | (0 << 5);
	}
|	spreg '>' '>' rcon
	{
		$$ = nullgen;
		$$.type = D_SHIFT;
		$$.offset = $1 | $4 | (1 << 5);
	}
|	spreg '-' '>' rcon
	{
		$$ = nullgen;
		$$.type = D_SHIFT;
		$$.offset = $1 | $4 | (2 << 5);
	}
|	spreg LAT '>' rcon
	{
		$$ = nullgen;
		$$.type = D_SHIFT;
		$$.offset = $1 | $4 | (3 << 5);
	}

rcon:
	spreg
	{
		if($$ < 0 || $$ >= 16)
			print("register value out of range\n");
		$$ = (($1&15) << 8) | (1 << 4);
	}

sreg:
	LREG
|	LPC
	{
		$$ = REGPC;
	}
|	LR '(' expr ')'
	{
		if($3 < 0 || $3 >= NREG)
			print("register value out of range\n");
		$$ = $3;
	}

spreg:
	sreg
|	LSP
	{
		$$ = REGSP;
	}

creg:
	LCREG
|	LC '(' expr ')'
	{
		if($3 < 0 || $3 >= NREG)
			print("register value out of range\n");
		$$ = $3;
	}

frcon:
	freg
|	fcon

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

name:
	con '(' pointer ')'
	{
		$$ = nullgen;
		$$.type = D_OREG;
		$$.name = $3;
		$$.sym = S;
		$$.offset = $1;
	}

con:
|	LVAR
	{
		$$ = $1->value;
	}

oexpr:
	{
		$$ = 0;
	}
|	',' expr
	{
		$$ = $2;
	}
