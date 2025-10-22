#define	NREG		16

#define	ALLTHUMBS	(1<<2)

#define	REGRET		0
#define	REGARG		0
/* compiler allocates R1 up as temps */
/* compiler allocates register variables R2 up */
#define	REGMIN		2
#define	REGMAX		8
#define	REGEXT		10
/* compiler allocates external registers R10 down */
#define	REGTMP		11
#define	REGSB		12
#define	REGSP		13
#define	REGLINK		14
#define	REGPC		15

#define	REGTMPT		7	/* used by the loader for thumb code */

#define	NFREG		8
#define	FREGRET		0
#define	FREGEXT		7
#define	FREGTMP		15
/* compiler allocates register variables F0 up */
/* compiler allocates external registers F7 down */

enum	as
{
	AAND,
	AEOR,
	ASUB,
	ARSB,
	AADD,
	AADC,
	ASBC,
	ARSC,
	ATST,
	ATEQ,
	ACMP,
	ACMN,
	AORR,
	ABIC,

	AMVN,

	AB,
	ABL,

/* 
 * Do not reorder or fragment the conditional branch 
 * opcodes, or the predication code will break 
 */ 
	ABEQ,
	ABNE,
	ABCS,
	ABHS,
	ABCC,
	ABLO,
	ABMI,
	ABPL,
	ABVS,
	ABVC,
	ABHI,
	ABLS,
	ABGE,
	ABLT,
	ABGT,
	ABLE,

	AMOVWD,
	AMOVWF,
	AMOVDW,
	AMOVFW,
	AMOVFD,
	AMOVDF,
	AMOVF,
	AMOVD,

	ACMPF,
	ACMPD,
	AADDF,
	AADDD,
	ASUBF,
	ASUBD,
	AMULF,
	AMULD,
	ADIVF,
	ADIVD,
//	ASQRTF,
//	ASQRTD,

	ASRL,
	ASRA,
	ASLL,
	AMULU,
	ADIVU,
	AMUL,
	ADIV,
	AMOD,
	AMODU,

	AMOVB,
	AMOVBU,
	AMOVH,
	AMOVHU,
	AMOVW,
	AMOVM,
	ASWPBU,
	ASWPW,

	ARFE,
	ASWI,
	AMULA,

	AGOK,
	ADYNT,
	AINIT,
	ABCASE,
	ACASE,

	AEND,

	AMULL,
	AMULAL,
	AMULLU,
	AMULALU,

	ABX,
	ABXRET,

	ADWORD,

	/* moved here to preserve values of older identifiers */
	ASQRTF,
	ASQRTD,

	ALDREX,
	ASTREX,
	
	ALDREXD,
	ASTREXD,

};

/* scond byte */
#define	C_SCOND	((1<<4)-1)
#define	C_SBIT	(1<<4)
#define	C_PBIT	(1<<5)
#define	C_WBIT	(1<<6)
#define	C_FBIT	(1<<7)	/* psr flags-only */
#define	C_UBIT	(1<<7)	/* up bit */

/* type */
#define	D_SCONST	(D_NONE+9)
#define	D_PSR		(D_NONE+10)
#define	D_OCONST	(D_NONE+17)

#define	D_SHIFT		(D_NONE+19)
#define	D_FPCR		(D_NONE+20)
#define	D_REGREG	(D_NONE+21)
#define	D_ADDR		(D_NONE+22)
