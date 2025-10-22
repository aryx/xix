#define	NREG	32

#define	REGZERO		0

#define	REGRET		1
#define	REGARG		1

/* compiler allocates R1 up as temps */

/* compiler allocates register variables R3-R23 */
#define	REGEXT		25
/* compiler allocates external registers R25 down */

/* dont use R26 R27 */
#define	REGTMP		28
#define	REGSP		29
#define	REGSB		30
#define	REGLINK		31

#define	FREGRET		0
/* compiler allocates register variables F4-F22 */
/* compiler allocates external registers F22 down */
#define	FREGEXT		22

#define	FREGZERO	24	/* both float and double */
#define	FREGHALF	26	/* double */
#define	FREGONE		28	/* double */
#define	FREGTWO		30	/* double */

enum	as
{
	AABSD,
	AABSF,
	AABSW,

	AADD,
	AADDD,
	AADDF,
	AADDU,
	AADDW,

	AAND,

	ABEQ,
	ABFPF,
	ABFPT,
	ABGEZ,
	ABGEZAL,
	ABGTZ,
	ABLEZ,
	ABLTZ,
	ABLTZAL,
	ABNE,
	ABREAK,

	ACMPEQD,
	ACMPEQF,
	ACMPGED,
	ACMPGEF,
	ACMPGTD,
	ACMPGTF,

	ADIV,
	ADIVD,
	ADIVF,
	ADIVU,
	ADIVW,

	AJAL,
	AJMP,

	AMOVB,
	AMOVBU,
	AMOVD,
	AMOVDF,
	AMOVDW,
	AMOVF,
	AMOVFD,
	AMOVFW,
	AMOVH,
	AMOVHU,
	AMOVW,
	AMOVWD,
	AMOVWF,
	AMOVWL,
	AMOVWR,

	AMUL,
	AMULD,
	AMULF,
	AMULU,
	AMULW,


	ANEGD,
	ANEGF,
	ANEGW,

	ANOR,
	AOR,
	AREM,
	AREMU,

	ARFE,

	ASGT,
	ASGTU,
	ASLL,
	ASRA,
	ASRL,

	ASUB,
	ASUBD,
	ASUBF,
	ASUBU,
	ASUBW,

	ASYSCALL,


	ATLBP,
	ATLBR,
	ATLBWI,
	ATLBWR,

	AXOR,

	AEND,

	AMOVV,
	AMOVVL,
	AMOVVR,

	ASLLV,
	ASRAV,
	ASRLV,

	ADIVV,
	ADIVVU,
	AREMV,
	AREMVU,
	AMULV,
	AMULVU,
	AADDV,
	AADDVU,
	ASUBV,
	ASUBVU,

	ATRUNCFV,
	ATRUNCDV,
	ATRUNCFW,
	ATRUNCDW,

	AMOVWU,
	AMOVFV,
	AMOVDV,
	AMOVVF,
	AMOVVD,
};

/* type */
#define	D_HI	(D_NONE+10)
#define	D_LO	(D_NONE+11)
#define	D_FCREG	(D_NONE+14)
#define	D_MREG	(D_NONE+15)
#define	D_OCONST (D_NONE+17)
#define	D_VCONST (D_NONE+19)
