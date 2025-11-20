
/*
 * 5c/arm
 * Arm 7500
 */

#define	R0ISZERO	0

struct	Adr
{
    ...
	int32	offset2;
	char	type;
	char	etype;
};

#define	INDEXED	9

struct	Case
{
	Case*	link;
	int32	val;
	int32	label;
	char	def;
	char	isv;
};

struct	C1
{
	int32	val;
	int32	label;
};

struct	Multab
{
	int32	val;
	char	code[20];
};

struct	Hintab
{
	ushort	val;
	char	hint[10];
};

struct	Var
{
	int32	offset;
	Sym*	sym;
	char	name;
	char	etype;
};

struct	Reg
{
	int32	pc;
	int32	rpo;		/* reverse post ordering */

	Bits	set;
	Bits	use1;
	Bits	use2;

	Bits	refbehind;
	Bits	refahead;
	Bits	calbehind;
	Bits	calahead;
	Bits	regdiff;
	Bits	act;

	int32	regu;
	int32	loop;		/* could be shorter */


	Reg*	log5;
	int32	active;

	Reg*	p1;
	Reg*	p2;
	Reg*	p2link;
	Reg*	s1;
	Reg*	s2;
	Reg*	link;
	Prog*	prog;
};
#define	R	((Reg*)0)

#define	NRGN	600
struct	Rgn
{
	Reg*	enter;
	short	cost;
	short	varno;
	short	regno;
};


EXTERN	int32	breakpc;
EXTERN	int32	nbreak;
EXTERN	Case*	cases;
EXTERN	Node	constnode;
EXTERN	Node	fconstnode;
EXTERN	int32	continpc;
EXTERN	int32	curarg;
EXTERN	int32	cursafe;
EXTERN	Prog*	firstp;
EXTERN	int32	isbigendian;
EXTERN	Prog*	lastp;
EXTERN	int32	maxargsafe;
EXTERN	int	mnstring;
EXTERN	Multab	multab[20];
EXTERN	int	retok;
EXTERN	int	hintabsize;
EXTERN	Node*	nodrat;
EXTERN	Node*	nodret;
EXTERN	Node*	nodsafe;
EXTERN	int32	nrathole;
EXTERN	int32	nstring;
EXTERN	Prog*	p;
EXTERN	int32	pc;
EXTERN	Node	regnode;
EXTERN	char	string[NSNAME];
EXTERN	Sym*	symrathole;
EXTERN	Node	znode;
EXTERN	Prog	zprog;
EXTERN	char	reg[NREG+NFREG];
EXTERN	int32	exregoffset;
EXTERN	int32	exfregoffset;
EXTERN	int	suppress;

#define	BLOAD(r)	band(bnot(r->refbehind), r->refahead)
#define	BSTORE(r)	band(bnot(r->calbehind), r->calahead)
#define	LOAD(r)		(~r->refbehind.b[z] & r->refahead.b[z])
#define	STORE(r)	(~r->calbehind.b[z] & r->calahead.b[z])

#define	bset(a,n)	((a).b[(n)/32]&(1L<<(n)%32))

#define	CLOAD	4
#define	CREF	5
#define	CINF	1000
#define	LOOP	3

EXTERN	Rgn	region[NRGN];
EXTERN	Rgn*	rgp;
EXTERN	int	nregion;
EXTERN	int	nvar;

EXTERN	Bits	externs;
EXTERN	Bits	params;
EXTERN	Bits	consts;
EXTERN	Bits	addrs;

EXTERN	int32	regbits;
EXTERN	int32	exregbits;

EXTERN	int	change;

EXTERN	Reg*	firstr;
EXTERN	Reg*	lastr;
EXTERN	Reg	zreg;
EXTERN	Reg*	freer;
EXTERN	Var	var[NVAR];
EXTERN	int32*	idom;
EXTERN	Reg**	rpo2r;
EXTERN	int32	maxnr;

extern	Hintab	hintab[];
