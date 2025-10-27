void
main(int argc, char *argv[])
{
	case  'L':			/* for little-endian mips */
		thechar = '0';
		thestring = "spim";
		break;
}

itab[] =
{
	"HI",		LHI,	D_HI,
	"LO",		LLO,	D_LO,

	"MOVD",		LTYPE5, AMOVD,
	"MOVF",		LTYPE5, AMOVF,

	"MOVDF",	LTYPE5, AMOVDF,
	"MOVDW",	LTYPE5, AMOVDW,
	"MOVFD",	LTYPE5, AMOVFD,
	"MOVFW",	LTYPE5, AMOVFW,
	"MOVWD",	LTYPE5, AMOVWD,
	"MOVWF",	LTYPE5, AMOVWF,

	"ABSW",		LTYPED, AABSW,
	"NEGW",		LTYPED, ANEGW,
	"ADDW",		LTYPEE, AADDW,
	"SUBW",		LTYPEE, ASUBW,
	"DIVW",		LTYPEE, ADIVW,
	"MULW",		LTYPEE, AMULW,

	"BFPT",		LTYPEG, ABFPT,
	"BFPF",		LTYPEG, ABFPF,

	"SCHED",	LSCHED, 0,
	"NOSCHED",	LSCHED, 0x80,
};
