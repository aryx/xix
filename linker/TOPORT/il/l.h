
typedef vlong xlong;

struct	Adr
{
	union
	{
		long	u0offset;
		char*	u0sval;
		Ieee*	u0ieee;
		vlong*	u0vval;
	} u0;
	union
	{
		Auto*	u1autom;
		Sym*	u1sym;
	} u1;
	char	type;
	char	reg;
	char	name;
	char	class;
};

struct	Prog
{
    ...
	union
	{
		long	u0regused;
		Prog*	u0forwd;
	} u0;
};

struct	Sym
{
	short	become;
	short	frame;

	ushort	file;

	long	sig;

};
struct	Autom
{
	Sym*	asym;
	Auto*	link;
	long	aoffset;
	short	type;
};

struct	Optab
{
    ...
	char	type;
	char	ctype;
	char	size;
	char	op;
	char	func3;
	short	param;
};

struct	Count
{
	long	count;
	long	outof;
};

enum
{
	STEXT		= 1,
	SDATA1,
	SLEAF,
	SFILE,
	SCONST,
	SSTRING,

	C_NONE		= 0,
    ...
	C_CTLREG,
    ...
	C_ZREG,
	C_ZCON,
	C_SCON,
	C_UCON,
	C_LCON,
	C_VCON,
	C_FCON,
	C_SACON,
	C_SECON,
	C_LACON,
	C_LECON,
	C_SRCON,
	C_LRCON,
	C_SBRA,
	C_LBRA,
	C_SAUTO,
	C_ZOREG,
	C_SOREG,
	C_LOREG,
	C_LAUTO,
	C_SEXT,
	C_LEXT,
	C_GOK,

	NSCHED		= 20,

/* mark flags */
	FOLL		= 1<<0,
	LABEL		= 1<<1,
	LEAF		= 1<<2,
	SYNC		= 1<<3,
	BRANCH		= 1<<4,
	COMPR		= 1<<5,
	SPASS		= 1<<6,

	BIG		= 2048,
	STRINGSZ	= 200,
	MINSIZ		= 64,
	NENT		= 100,
	MAXIO		= 8192,
};

/* Major opcodes */
enum {
	OLOAD,	 OLOAD_FP,  Ocustom_0,	OMISC_MEM, OOP_IMM, OAUIPC, OOP_IMM_32,	O48b,
	OSTORE,	 OSTORE_FP, Ocustom_1,	OAMO,	   OOP,	    OLUI,   OOP_32,	O64b,
	OMADD,	 OMSUB,	    ONMSUB,	ONMADD,	   OOP_FP,  Ores_0, Ocustom_2,	O48b_2,
	OBRANCH, OJALR,	    Ores_1,	OJAL,	   OSYSTEM, Ores_2, Ocustom_3,	O80b
};


EXTERN	long	HEADR;			/* length of header */
EXTERN	int	HEADTYPE;		/* type of header */

EXTERN	xlong	INITDAT;		/* data location */
EXTERN	xlong	INITRND;		/* data round above text location */
EXTERN	xlong	INITTEXT;		/* text location */
EXTERN	xlong	INITTEXTP;		/* text location (physical) */
EXTERN	char*	INITENTRY;		/* entry point */

EXTERN	long	autosize;
EXTERN	Biobuf	bso;
EXTERN	long	bsssize;
EXTERN	int	cbc;
EXTERN	uchar*	cbp;
EXTERN	int	cout;
EXTERN	Auto*	curauto;
EXTERN	Auto*	curhist;
EXTERN	Prog*	curp;
EXTERN	Prog*	curtext;
EXTERN	Prog*	datap;
EXTERN	long	datsize;
EXTERN	char	debug[128];
EXTERN	Prog*	etextp;
EXTERN	Prog*	firstp;
EXTERN	char	fnuxi4[4];	/* for 3l [sic] */
EXTERN	char	fnuxi8[8];
EXTERN	char*	noname;
EXTERN	Sym*	hash[NHASH];
EXTERN	Sym*	histfrog[MAXHIST];
EXTERN	int	histfrogp;
EXTERN	int	histgen;
EXTERN	char*	library[50];
EXTERN	char*	libraryobj[50];
EXTERN	int	libraryp;
EXTERN	int	xrefresolv;
EXTERN	char*	hunk;
EXTERN	char	inuxi1[1];
EXTERN	char	inuxi2[2];
EXTERN	char	inuxi4[4];
EXTERN	char	inuxi8[8];
EXTERN	Prog*	lastp;
EXTERN	long	lcsize;
EXTERN	char	literal[32];
EXTERN	int	nerrors;
EXTERN	long	nhunk;
EXTERN	Prog	nopalign;
EXTERN	long	instoffset;
EXTERN	vlong	instoffx;
EXTERN	Opcross	opcross[10];
EXTERN	Oprang	oprange[ALAST];
EXTERN	char*	outfile;
EXTERN	long	pc;
EXTERN	int	ptrsize;
EXTERN	uchar	repop[ALAST];
EXTERN	long	symsize;
EXTERN	Prog*	textp;
EXTERN	long	textsize;
EXTERN	long	thunk;
EXTERN	int	version;
EXTERN	char	xcmp[32][32];
EXTERN	Prog	zprg;
EXTERN	int	dtype;
EXTERN	int	little;

EXTERN	struct
{
	Count	branch;
	Count	fcmp;
	Count	load;
	Count	mfrom;
	Count	page;
	Count	jump;
} nop;

int	asmcompressed(Prog*, Optab*, int, int);
...
