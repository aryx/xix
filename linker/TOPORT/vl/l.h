
struct	Autom
{
	Sym*	asym;
	long	aoffset;
	short	type;
};


struct	Count
{
	long	count;
	long	outof;
};

enum
{
	SDATA1,
	SLEAF,
	SFILE,
	SCONST,
	SSTRING,

	C_NONE		= 0,
	C_FREG,
	C_FCREG,
	C_MREG,
	C_HI,
	C_LO,
	C_ZCON,
	C_SCON,
	C_ADD0CON,
	C_AND0CON,
	C_ADDCON,
	C_ANDCON,
	C_UCON,
	C_LCON,
	C_SACON,
	C_SECON,
	C_LACON,
	C_LECON,
	C_SBRA,
	C_LBRA,
	C_SAUTO,
	C_SEXT,
	C_LAUTO,
	C_LEXT,
	C_ZOREG,
	C_SOREG,
	C_LOREG,

	NSCHED		= 20,

/* mark flags */
	FOLL		= 1<<0,
	LABEL		= 1<<1,
	SYNC		= 1<<3,
	BRANCH		= 1<<4,
	LOAD		= 1<<5,
	FCMP		= 1<<6,
	NOSCHED		= 1<<7,

	BIG		= 32766,
	STRINGSZ	= 200,
	MINSIZ		= 64,
	NENT		= 100,
};


EXTERN	char	fnuxi4[4];	/* for 3l [sic] */
EXTERN	char	fnuxi8[8];

EXTERN	char	inuxi1[1];
EXTERN	char	inuxi2[2];
EXTERN	char	inuxi4[4];

EXTERN	char	literal[32];

EXTERN	uchar	repop[ALAST];

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
