enum
{
	PtrSize = 4
};

/* do not undefine this - code will be removed eventually */
#define	CALLEEBX

struct	Adr
{
	union
	{
		Ieee	u0ieee;
		char*	u0sbig;
	} u0;
    ...
	uchar	index; // not used on arm, required by ld/go.c
	int32	offset2; // argsize
	char	class;
	Sym*	gotype;
};



struct	Reloc
{
	int32	off;
	uchar	siz;
	uchar	type;
	int32	add;
	Sym*	sym;
};

struct	Prog
{
	union
	{
		int32	u0regused;
	} u0;
    ...
	Prog*	dlink;
	uchar	align;
};

struct	Sym
{

	uchar	dupok;
	uchar	reachable;
	uchar	dynexport;
	uchar	leaf;


	int32	size;
	uchar	used;

	uchar	thumb;	// thumb code
	uchar	foreign;	// called by arm if thumb, by thumb if arm
	uchar	fnptr;	// used as fn ptr
	Use*		use;

	Sym*	gotype;

	char*	file;

	char*	dynimpname;
	char*	dynimplib;
	
	// STEXT
	Auto*	autom;
	Prog*	text;
	
	// SDATA, SBSS
	uchar*	p;
	int32	np;
	int32	maxp;
	Reloc*	r;
	int32	nr;
	int32	maxr;
};

#define SIGNINTERN	(1729*325*1729)

struct	Autom
{
	Sym*	asym;
	Auto*	link;
	int32	aoffset;
	short	type;
	Sym*	gotype;
};


struct	Count
{
	int32	count;
	int32	outof;
};

struct	Use
{
	Prog*	p;	/* use */
	Prog*	ct;	/* curtext */
	Use*		link;
};

enum
{
	Sxxx,
	
	/* order here is order in output file */
	SRODATA,
	SELFDATA,

	SFILE,
	SCONST,
	SFIXED,

	LFROM		= 1<<0,
	LTO		= 1<<1,
	LPOOL		= 1<<2,
	V4		= 1<<3,	/* arm v4 arch */

	C_NONE		= 0,
	C_REG,
	C_REGREG,
	C_SHIFT,
	C_FREG,
	C_PSR,
	C_FCR,

	C_RCON,		/* 0xff rotated */
	C_NCON,		/* ~RCON */
	C_SCON,		/* 0xffff */
	C_BCON,		/* thumb */
	C_LCON,
	C_FCON,
	C_GCON,		/* thumb */

	C_RACON,
	C_SACON,	/* thumb */
	C_LACON,
	C_GACON,	/* thumb */

	C_SBRA,
	C_LBRA,
	C_GBRA,		/* thumb */

	C_HAUTO,	/* halfword insn offset (-0xff to 0xff) */
	C_FAUTO,	/* float insn offset (0 to 0x3fc, word aligned) */
	C_HFAUTO,	/* both H and F */
	C_SAUTO,	/* -0xfff to 0xfff */
	C_LAUTO,

	C_HOREG,
	C_FOREG,
	C_HFOREG,
	C_SOREG,
	C_ROREG,
	C_SROREG,	/* both S and R */
	C_LOREG,
	C_GOREG,		/* thumb */

	C_PC,
	C_SP,
	C_HREG,
	C_OFFPC,		/* thumb */

	C_ADDR,		/* relocatable address */

	C_GOK,

/* mark flags */
	FOLL		= 1<<0,
	LABEL		= 1<<1,
	LEAF		= 1<<2,

	MINSIZ		= 64,
	NENT		= 100,
	MINLC	= 4,
};


#define	setarch(p)		if((p)->as==ATEXT) thumb=(p)->reg&ALLTHUMBS
#define	setthumb(p)	if((p)->as==ATEXT) seenthumb|=(p)->reg&ALLTHUMBS

#ifndef COFFCVT

EXTERN	int32	autosize;


EXTERN	int32 	elfdatsize;


EXTERN	char*	noname;
EXTERN	int	xrefresolv;

EXTERN	int32	lcsize;
EXTERN	char	literal[32];
EXTERN	int	nerrors;
EXTERN	int32	instoffset;

EXTERN	Oprang	thumboprange[ALAST];


EXTERN	uchar	repop[ALAST];
EXTERN	char*	rpath;
EXTERN	uint32	stroffset;
EXTERN	int32	symsize;

EXTERN	int	dtype;

EXTERN	int	armv4;
EXTERN	int	thumb;
EXTERN	int	seenthumb;
EXTERN	int	armsize;

extern	char*	anames[];

EXTERN	Prog*	blitrl;
EXTERN	Prog*	elitrl;

EXTERN	Prog*	prog_div;
EXTERN	Prog*	prog_divu;
EXTERN	Prog*	prog_mod;
EXTERN	Prog*	prog_modu;
#define	VPUT(a)	abort()
