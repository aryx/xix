enum
{
	SDATA1,
	SLEAF,
	SFILE,
	SCONST,
	SSTRING,

	C_NONE		= 0,
	C_FREG, // 1
	C_FCREG,
	C_MREG,
	C_HI,
	C_LO, // 5
	C_ZCON,
	C_SCON,
	C_ADD0CON,
	C_AND0CON,
	C_ADDCON, // 10
	C_ANDCON,
	C_UCON,
	C_LCON,
	C_SACON,
	C_SECON, // 15
	C_LACON,
	C_LECON,
	C_SBRA,
	C_LBRA,
	C_SAUTO, // 20
	C_SEXT,
	C_LAUTO,
	C_LEXT,
	C_ZOREG,
	C_SOREG, // 25
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

};


EXTERN	char	literal[32];

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
