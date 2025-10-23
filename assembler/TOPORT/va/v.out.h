#define	REGZERO		0

#define	REGRET		1
#define	REGARG		1

/* compiler allocates R1 up as temps */

/* compiler allocates register variables R3-R23 */
#define	REGEXT		25
/* compiler allocates external registers R25 down */

/* dont use R26 R27 */

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
	AABSW,
	AABSD,
	AABSF,

	AADDW,
	AADDD,
	AADDF,

	ABFPF,
	ABFPT,

	ACMPEQD,
	ACMPEQF,
	ACMPGED,
	ACMPGEF,
	ACMPGTD,
	ACMPGTF,

	ADIVW,
	ADIVD,
	ADIVF,

	AMOVB,
	AMOVBU,
	AMOVH,
	AMOVHU,
	AMOVW,

	AMOVD,
	AMOVDF,
	AMOVDW,
	AMOVF,
	AMOVFD,
	AMOVFW,

	AMOVWD,
	AMOVWF,
	AMOVWL,
	AMOVWR,

	AMULW,
	AMULD,
	AMULF,

	ANEGW,
	ANEGD,
	ANEGF,

	ASUBW,
	ASUBD,
	ASUBF,

	AMOVV,
	AMOVVL,
	AMOVVR,

	AREMV,
	AREMVU,

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
