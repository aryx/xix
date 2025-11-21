%union
{
	vlong	lval;
}
%token	<lval>	LADD LMUL LBEQ LBR LBRET LCALL LFLT2 LFLT3
%token	<lval>	LMOVB LMOVBU LMOVW LMOVF LLUI LSYS LSYS0 LCSR LSWAP LAMO
%token	<lval>	LCTL
%token	<dval>	LFCONST
%type	<lval>	sreg freg oprrr
%type	<gen>	rreg dreg ctlreg drreg
%type	<gen>	addr
%%

inst:
	LADD imm ',' rreg
	{
		outcode($1, &$2, NREG, &$4);
	}
|	oprrr rreg ',' rreg
	{
		outcode($1, &$2, NREG, &$4);
	}
|	LADD imm ',' sreg ',' rreg
	{
		outcode($1, &$2, $4, &$6);
	}
|	oprrr rreg ',' sreg ',' rreg
	{
		outcode($1, &$2, $4, &$6);
	}


|	LFLT2 drreg ',' drreg
	{
		outcode($1, &$2, NREG, &$4);
	}
|	LFLT3 drreg ',' freg ',' drreg
	{
		outcode($1, &$2, $4, &$6);
	}

|	LBEQ rreg ',' sreg ',' rel
	{
		outcode($1, &$2, $4, &$6);
	}

|	LBEQ rreg ',' rel
	{
		Gen regzero;
		regzero = nullgen;
		regzero.type = D_REG;
		regzero.reg = 0;
		outcode($1, &regzero, $2.reg, &$4);
	}

|	LBR	rel
	{
		outcode($1, &nullgen, NREG, &$2);
	}

|	LBR oreg
	{
		outcode($1, &nullgen, NREG, &$2);
	}


|	LBRET
	{
		outcode($1, &nullgen, NREG, &nullgen);
	}


|	LCALL sreg ',' addr
	{
		outcode($1, &nullgen, $2, &$4);
	}
|	LCALL sreg ',' rel
	{
		outcode($1, &nullgen, $2, &$4);
	}

|	LMOVB addr ',' rreg
	{
		outcode($1, &$2, NREG, &$4);
	}
|	LMOVBU addr ',' rreg
	{
		outcode($1, &$2, NREG, &$4);
	}
|	LMOVB rreg ',' addr
	{
		outcode($1, &$2, NREG, &$4);
	}



|	LMOVF addr ',' dreg
	{
		outcode($1, &$2, NREG, &$4);
	}
|	LMOVF dreg ',' addr
	{
		outcode($1, &$2, NREG, &$4);
	}
|	LMOVF dreg ',' dreg
	{
		outcode($1, &$2, NREG, &$4);
	}

		
|	LMOVW imm ',' rreg
	{
		outcode($1, &$2, NREG, &$4);
	}
|	LMOVW ximm ',' rreg
	{
		outcode($1, &$2, NREG, &$4);
	}
|	LMOVW rreg ',' rreg
	{
		outcode($1, &$2, NREG, &$4);
	}
|	LMOVW addr ',' rreg
	{
		outcode($1, &$2, NREG, &$4);
	}
|	LMOVW rreg ',' addr
	{
		outcode($1, &$2, NREG, &$4);
	}
|	LMOVW rreg ',' ctlreg
	{
		Gen regzero;
		regzero = nullgen;
		regzero.type = D_REG;
		regzero.reg = 0;
		outcode(ACSRRW, &$4, $2.reg, &regzero);
	}
|	LMOVW imm ',' ctlreg
	{
		Gen regzero;
		int r = $2.offset;
		if(r < 0 || r >= NREG)
			yyerror("immediate value out of range");
		regzero = nullgen;
		regzero.type = D_REG;
		regzero.reg = 0;
		outcode(ACSRRWI, &$4, r, &regzero);
	}
|	LMOVW ctlreg ',' rreg
	{
		outcode(ACSRRS, &$2, REGZERO, &$4);
	}

|	LLUI name ',' rreg
	{
		outcode($1, &$2, NREG, &$4);
	}
|	LLUI imm ',' rreg
	{
		outcode($1, &$2, NREG, &$4);
	}


|	LSYS imm
	{
		outcode($1, &nullgen, NREG, &$2);
	}

|	LSYS0
	{
		Gen syscon;
		syscon = nullgen;
		syscon.type = D_CONST;
		syscon.offset = $1;
		outcode(ASYS, &nullgen, NREG, &syscon);
	}

|	LCSR ctlreg ',' sreg ',' rreg
	{
		outcode($1, &$2, $4, &$6);
	}

|	LCSR ctlreg ',' '$' con ',' rreg
	{
		if($5 < 0 || $5 >= NREG)
			yyerror("immediate value out of range");
		outcode($1 + (ACSRRWI-ACSRRW), &$2, $5, &$7);
	}

|	LSWAP rreg ',' sreg ',' rreg
	{
		outcode($1, &$2, $4, &$6);
	}

|	LAMO con ',' rreg ',' sreg ',' rreg
	{
		outcode($1, &$4, ($2<<16)|$6, &$8);
	}





oprrr:
	LADD
|	LMUL

addr:
	oreg
|   ...

oreg:
	'(' sreg ')'
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
    ...

rreg:
	sreg
	{
		$$ = nullgen;
		$$.type = D_REG;
		$$.reg = $1;
	}

dreg:
	freg
	{
		$$ = nullgen;
		$$.type = D_FREG;
		$$.reg = $1;
	}

drreg:
	dreg
|	rreg
	
sreg:
	LREG
|	LR '(' expr ')'
	{
		if($3 < 0 || $3 >= NREG)
			yyerror("register value out of range");
		$$ = $3;
	}

freg:
	LFREG
|	FR '(' expr ')'
	{
		if($3 < 0 || $3 >= NREG)
			yyerror("register value out of range");
		$$ = $3;
	}

ctlreg:
	LCTL '(' expr ')'
	{
		if($3 < 0 || $3 >= 0xFFF)
			yyerror("CSR value out of range");
		$$ = nullgen;
		$$.type = D_CTLREG;
		$$.offset = $3;
	}

imm:
	'$' con
	{
		if(thechar == 'j' && (vlong)$$.offset != $2){
			$$.type = D_VCONST;
			$$.vval = $2;
		}
	}
