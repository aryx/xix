void
main(int argc, char *argv[])
{
	thechar = '5';
	thestring = "arm";

	case 't':
		thechar = 't';
		thestring = "thumb";
		break;
	} ARGEND
}

itab[] =
{
	"C",		LC,	0,

	"C0",		LCREG,	0,
	"C1",		LCREG,	1,
	"C2",		LCREG,	2,
	"C3",		LCREG,	3,
	"C4",		LCREG,	4,
	"C5",		LCREG,	5,
	"C6",		LCREG,	6,
	"C7",		LCREG,	7,
	"C8",		LCREG,	8,
	"C9",		LCREG,	9,
	"C10",		LCREG,	10,
	"C11",		LCREG,	11,
	"C12",		LCREG,	12,
	"C13",		LCREG,	13,
	"C14",		LCREG,	14,
	"C15",		LCREG,	15,

	"CPSR",		LPSR,	0,
	"SPSR",		LPSR,	1,

	"FPSR",		LFCR,	0,
	"FPCR",		LFCR,	1,

	".EQ",		LCOND,	0,
	".NE",		LCOND,	1,
	".CS",		LCOND,	2,
	".HS",		LCOND,	2,
	".CC",		LCOND,	3,
	".LO",		LCOND,	3,
	".MI",		LCOND,	4,
	".PL",		LCOND,	5,
	".VS",		LCOND,	6,
	".VC",		LCOND,	7,
	".HI",		LCOND,	8,
	".LS",		LCOND,	9,
	".GE",		LCOND,	10,
	".LT",		LCOND,	11,
	".GT",		LCOND,	12,
	".LE",		LCOND,	13,
	".AL",		LCOND,	Always,

	".U",		LS,	C_UBIT,
	".S",		LS,	C_SBIT,
	".W",		LS,	C_WBIT,
	".P",		LS,	C_PBIT,
	".PW",		LS,	C_WBIT|C_PBIT,
	".WP",		LS,	C_WBIT|C_PBIT,

	".F",		LS,	C_FBIT,

	".IBW",		LS,	C_WBIT|C_PBIT|C_UBIT,
	".IAW",		LS,	C_WBIT|C_UBIT,
	".DBW",		LS,	C_WBIT|C_PBIT,
	".DAW",		LS,	C_WBIT,
	".IB",		LS,	C_PBIT|C_UBIT,
	".IA",		LS,	C_UBIT,
	".DB",		LS,	C_PBIT,
	".DA",		LS,	0,

	"@",		LAT,	0,

	"AND",		LTYPE1,	AAND,
	"EOR",		LTYPE1,	AEOR,
	"SUB",		LTYPE1,	ASUB,
	"RSB",		LTYPE1,	ARSB,
	"ADD",		LTYPE1,	AADD,
	"ADC",		LTYPE1,	AADC,
	"SBC",		LTYPE1,	ASBC,
	"RSC",		LTYPE1,	ARSC,
	"ORR",		LTYPE1,	AORR,
	"BIC",		LTYPE1,	ABIC,

	"SLL",		LTYPE1,	ASLL,
	"SRL",		LTYPE1,	ASRL,
	"SRA",		LTYPE1,	ASRA,

	"MUL",		LTYPE1, AMUL,
	"MULA",		LTYPEN, AMULA,
	"DIV",		LTYPE1,	ADIV,
	"MOD",		LTYPE1,	AMOD,

	"MULL",		LTYPEM, AMULL,
	"MULAL",	LTYPEM, AMULAL,
	"MULLU",	LTYPEM, AMULLU,
	"MULALU",	LTYPEM, AMULALU,

	"MVN",		LTYPE2, AMVN,	/* op2 ignored */

	"MOVB",		LTYPE3, AMOVB,
	"MOVBU",	LTYPE3, AMOVBU,
	"MOVH",		LTYPE3, AMOVH,
	"MOVHU",	LTYPE3, AMOVHU,
	"MOVW",		LTYPE3, AMOVW,

	"MOVD",		LTYPE3, AMOVD,
	"MOVDF",		LTYPE3, AMOVDF,
	"MOVDW",	LTYPE3, AMOVDW,
	"MOVF",		LTYPE3, AMOVF,
	"MOVFD",		LTYPE3, AMOVFD,
	"MOVFW",		LTYPE3, AMOVFW,
	"MOVWD",	LTYPE3, AMOVWD,
	"MOVWF",		LTYPE3, AMOVWF,

/*
	"ABSF",		LTYPEI, AABSF,
	"ABSD",		LTYPEI, AABSD,
	"NEGF",		LTYPEI, ANEGF,
	"NEGD",		LTYPEI, ANEGD,
	"SQTF",		LTYPEI,	ASQTF,
	"SQTD",		LTYPEI,	ASQTD,
	"RNDF",		LTYPEI,	ARNDF,
	"RNDD",		LTYPEI,	ARNDD,
	"URDF",		LTYPEI,	AURDF,
	"URDD",		LTYPEI,	AURDD,
	"NRMF",		LTYPEI,	ANRMF,
	"NRMD",		LTYPEI,	ANRMD,
*/

	"CMPF",		LTYPEL, ACMPF,
	"CMPD",		LTYPEL, ACMPD,
	"ADDF",		LTYPEK,	AADDF,
	"ADDD",		LTYPEK,	AADDD,
	"SUBF",		LTYPEK,	ASUBF,
	"SUBD",		LTYPEK,	ASUBD,
	"MULF",		LTYPEK,	AMULF,
	"MULD",		LTYPEK,	AMULD,
	"DIVF",		LTYPEK,	ADIVF,
	"DIVD",		LTYPEK,	ADIVD,

	"B",		LTYPE4, AB,
	"BL",		LTYPE4, ABL,
	"BX",		LTYPEBX,	ABX,

	"BEQ",		LTYPE5,	ABEQ,
	"BNE",		LTYPE5,	ABNE,
	"BCS",		LTYPE5,	ABCS,
	"BHS",		LTYPE5,	ABHS,
	"BCC",		LTYPE5,	ABCC,
	"BLO",		LTYPE5,	ABLO,
	"BMI",		LTYPE5,	ABMI,
	"BPL",		LTYPE5,	ABPL,
	"BVS",		LTYPE5,	ABVS,
	"BVC",		LTYPE5,	ABVC,
	"BHI",		LTYPE5,	ABHI,
	"BLS",		LTYPE5,	ABLS,
	"BGE",		LTYPE5,	ABGE,
	"BLT",		LTYPE5,	ABLT,
	"BGT",		LTYPE5,	ABGT,
	"BLE",		LTYPE5,	ABLE,

	"BCASE",	LTYPE5,	ABCASE,

	"SWI",		LTYPE6, ASWI,

	"CMP",		LTYPE7,	ACMP,
	"TST",		LTYPE7,	ATST,
	"TEQ",		LTYPE7,	ATEQ,
	"CMN",		LTYPE7,	ACMN,

	"MOVM",		LTYPE8, AMOVM,

	"SWPBU",	LTYPE9, ASWPBU,
	"SWPW",		LTYPE9, ASWPW,

	"RFE",		LTYPEA, ARFE,

	"CASE",		LTYPED, ACASE,
	"END",		LTYPEE, AEND,

	"MCR",		LTYPEJ, 0,
	"MRC",		LTYPEJ, 1,
	0
};

void
outcode(int a, int scond, Gen *g1, int reg, Gen *g2)
{
	int sf, st, t;
	Sym *s;

	/* hack to make B.NE etc. work: turn it into the corresponding conditional */
	if(a == AB){
		a = bcode[scond&0xf];
		scond = (scond & ~0xf) | Always;
	}
}
