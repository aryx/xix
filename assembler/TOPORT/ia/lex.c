void
main(int argc, char *argv[])
{

	thechar = 'i';
	p = strrchr(argv[0], '/');
	if(p == nil)
		p = argv[0];
	else
		p++;
	if(*p == 'j')
		thechar = 'j';

	if(debug['j'])
		thechar = 'j';
	thestring = (thechar == 'j'? "riscv64" : "riscv");
}

int
assemble(char *file)
{
	if(thechar == 'j')
		dodefine("XLEN=8");
	else
		dodefine("XLEN=4");

	Bprint(&obuf, "\n!\n");

	if(thechar == 'j')
		dodefine("XLEN=8");
	else
		dodefine("XLEN=4");
}

itab[] =
{
	"CSR",		LCTL,	0,

	"ADD",		LADD,	AADD,
	"SLL",		LADD,	ASLL,
	"SLT",		LADD,	ASLT,
	"SLTU",		LADD,	ASLTU,
	"XOR",		LADD,	AXOR,
	"SRL",		LADD,	ASRL,
	"OR",		LADD,	AOR,
	"AND",		LADD,	AAND,
	"SRA",		LADD,	ASRA,
	"ADDW",		LADD,	AADDW,
	"SLLW",		LADD,	ASLLW,
	"SRLW",		LADD,	ASRLW,
	"SRAW",		LADD,	ASRAW,
	"SUB",		LADD,	ASUB,
	"SUBW",		LADD,	ASUBW,

	"MUL",		LMUL,	AMUL,
	"MULH",		LMUL,	AMULH,
	"MULHSU",	LMUL,	AMULHSU,
	"MULHU",	LMUL,	AMULHU,
	"DIV",		LMUL,	ADIV,
	"DIVU",		LMUL,	ADIVU,
	"REM",		LMUL,	AREM,
	"REMU",		LMUL,	AREMU,
	"MULW",		LMUL,	AMULW,
	"DIVW",		LMUL,	ADIVW,
	"DIVUW",	LMUL,	ADIVUW,
	"REMW",		LMUL,	AREMW,
	"REMUW",	LMUL,	AREMUW,

	"BEQ",		LBEQ,	ABEQ,
	"BNE",		LBEQ,	ABNE,
	"BLT",		LBEQ,	ABLT,
	"BGE",		LBEQ,	ABGE,
	"BLTU",		LBEQ,	ABLTU,
	"BGEU",		LBEQ,	ABGEU,
	"BGT",		LBEQ,	ABGT,
	"BGTU",		LBEQ,	ABGTU,
	"BLE",		LBEQ,	ABLE,
	"BLEU",		LBEQ,	ABLEU,

	"JMP",		LBR,	AJMP,

	"FENCE_I",	LBRET,	AFENCE_I,

	"END",		LBRET,	AEND,

	"JAL",		LCALL,	AJAL,
	"JALR",		LCALL,	AJAL,

	"MOVB",		LMOVB,	AMOVB,
	"MOVH",		LMOVB,	AMOVH,

	"MOVF",		LMOVF,	AMOVF,
	"MOVD",		LMOVF,	AMOVD,

	"MOVBU",	LMOVBU,	AMOVBU,
	"MOVHU",	LMOVBU,	AMOVHU,

	"MOVW",		LMOVW,	AMOVW,

	"MOVFD",	LFLT2,	AMOVFD,
	"MOVDF",	LFLT2,	AMOVDF,
	"MOVWF",	LFLT2,	AMOVWF,
	"MOVUF",	LFLT2,	AMOVUF,
	"MOVFW",	LFLT2,	AMOVFW,
	"MOVWD",	LFLT2,	AMOVWD,
	"MOVUD",	LFLT2,	AMOVUD,
	"MOVDW",	LFLT2,	AMOVDW,

	"ADDF",		LFLT3,	AADDF,
	"ADDD",		LFLT3,	AADDD,
	"SUBF",		LFLT3,	ASUBF,
	"SUBD",		LFLT3,	ASUBD,
	"MULF",		LFLT3,	AMULF,
	"MULD",		LFLT3,	AMULD,
	"DIVF",		LFLT3,	ADIVF,
	"DIVD",		LFLT3,	ADIVD,

	"CMPLTF",	LFLT3,	ACMPLTF,
	"CMPLTD",	LFLT3,	ACMPLTD,
	"CMPEQF",	LFLT3,	ACMPEQF,
	"CMPEQD",	LFLT3,	ACMPEQD,
	"CMPLEF",	LFLT3,	ACMPLEF,
	"CMPLED",	LFLT3,	ACMPLED,

	"LUI",		LLUI,	ALUI,

	"SYS",		LSYS,	ASYS,

	"ECALL",	LSYS0,	0,
	"EBREAK",	LSYS0,	1,

	"CSRRW",	LCSR,	ACSRRW,
	"CSRRS",	LCSR,	ACSRRS,
	"CSRRC",	LCSR,	ACSRRC,

	"SWAP_W",	LSWAP,	ASWAP_W,
	"SWAP_D",	LSWAP,	ASWAP_D,

	"LR_W",		LSWAP,	ALR_W,
	"LR_D",		LSWAP,	ALR_D,
	"SC_W",		LSWAP,	ASC_W,
	"SC_D",		LSWAP,	ASC_D,

	"AMO_W",	LAMO,	AAMO_W,
	"AMO_D",	LAMO,	AAMO_D,

	"DWORD",	LWORD,		ADWORD,

	"MOV",		LMOVW,		AMOV,
	"MOVWU",	LMOVBU,		AMOVWU,
};


void
zaddr(Gen *a, int s)
{
	vlong v;

	switch(a->type) {
	case D_VCONST:
		v = a->vval;
		Bputc(&obuf, v);
		Bputc(&obuf, v>>8);
		Bputc(&obuf, v>>16);
		Bputc(&obuf, v>>24);
		Bputc(&obuf, v>>32);
		Bputc(&obuf, v>>40);
		Bputc(&obuf, v>>48);
		Bputc(&obuf, v>>56);
		break;
}
