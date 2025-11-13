
typedef vlong xlong;

struct	Adr
{
	union
	{
		long	u0offset;
		Ieee*	u0ieee;
		vlong*	u0vval;
	} u0;
    ...
};

struct	Prog
{
    ...
	union
	{
		long	u0regused;
        ...
	} u0;
};

struct	Sym
{
	short	become;
	short	frame;

	ushort	file;
    ...
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


enum
{
	SDATA1,

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
    ...
	COMPR		= 1<<5,
	SPASS		= 1<<6,

	//BIG		= 2048,
	MAXIO		= 8192,
};

/* Major opcodes */
enum {
	OLOAD,	 OLOAD_FP,  Ocustom_0,	OMISC_MEM, OOP_IMM, OAUIPC, OOP_IMM_32,	O48b,
	OSTORE,	 OSTORE_FP, Ocustom_1,	OAMO,	   OOP,	    OLUI,   OOP_32,	O64b,
	OMADD,	 OMSUB,	    ONMSUB,	ONMADD,	   OOP_FP,  Ores_0, Ocustom_2,	O48b_2,
	OBRANCH, OJALR,	    Ores_1,	OJAL,	   OSYSTEM, Ores_2, Ocustom_3,	O80b
};


EXTERN	xlong	INITDAT;		/* data location */
EXTERN	xlong	INITRND;		/* data round above text location */
EXTERN	xlong	INITTEXT;		/* text location */
EXTERN	xlong	INITTEXTP;		/* text location (physical) */

...
EXTERN	Prog	nopalign;
...
EXTERN	vlong	instoffx;
EXTERN	int	ptrsize;
...
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
