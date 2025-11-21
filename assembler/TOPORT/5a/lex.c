void
main(int argc, char *argv[])
{
	case 't':
		thechar = 't';
		thestring = "thumb";
		break;
	}
}

itab[] =
{
	"CPSR",		LPSR,	0,
	"SPSR",		LPSR,	1,

	"FPSR",		LFCR,	0,
	"FPCR",		LFCR,	1,

	".CS",		LCOND,	2,
	".CC",		LCOND,	3,

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

	"MULA",		LTYPEN, AMULA,

	"MULL",		LTYPEM, AMULL,
	"MULAL",	LTYPEM, AMULAL,
	"MULLU",	LTYPEM, AMULLU,
	"MULALU",	LTYPEM, AMULALU,


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

	"BX",		LTYPEBX,	ABX,

	"BCS",		LTYPE5,	ABCS,
	"BCC",		LTYPE5,	ABCC,

	"BCASE",	LTYPE5,	ABCASE,

	"MOVM",		LTYPE8, AMOVM,

	"SWPBU",	LTYPE9, ASWPBU,
	"SWPW",		LTYPE9, ASWPW,

	"MCR",		LTYPEJ, 0,
	"MRC",		LTYPEJ, 1,
};

void
outcode(int a, int scond, Gen *g1, int reg, Gen *g2)
{
	/* hack to make B.NE etc. work: turn it into the corresponding conditional */
	if(a == AB){
		a = bcode[scond&0xf];
		scond = (scond & ~0xf) | Always;
	}
}
