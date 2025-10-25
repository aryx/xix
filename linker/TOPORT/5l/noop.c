// Inferno utils/5l/noop.c
// http://code.google.com/p/inferno-os/source/browse/utils/5l/noop.c
//
//	Copyright © 1994-1999 Lucent Technologies Inc.  All rights reserved.
//	Portions Copyright © 1995-1997 C H Forsyth (forsyth@terzarima.net)
//	Portions Copyright © 1997-1999 Vita Nuova Limited
//	Portions Copyright © 2000-2007 Vita Nuova Holdings Limited (www.vitanuova.com)
//	Portions Copyright © 2004,2006 Bruce Ellis
//	Portions Copyright © 2005-2007 C H Forsyth (forsyth@terzarima.net)
//	Revisions Copyright © 2000-2007 Lucent Technologies Inc. and others
//	Portions Copyright © 2009 The Go Authors.  All rights reserved.
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
// THE SOFTWARE.

// Code transformations.

#include	"l.h"
#include	"../ld/lib.h"

// see ../../runtime/proc.c:/StackGuard
enum
{
	StackBig = 4096,
};

static	Sym*	sym_div;
static	Sym*	sym_divu;
static	Sym*	sym_mod;
static	Sym*	sym_modu;

static void setdiv(int);

static Prog *
movrr(Prog *q, int rs, int rd, Prog *p)
{
	if(q == nil)
		q = prg();
	q->as = AMOVW;
	q->line = p->line;
	q->from.type = D_REG;
	q->from.reg = rs;
	q->to.type = D_REG;
	q->to.reg = rd;
	q->link = p->link;
	return q;
}

static Prog *
fnret(Prog *q, int rs, int foreign, Prog *p)
{
	q = movrr(q, rs, REGPC, p);
	if(foreign){	// BX rs
		q->as = ABXRET;
		q->from.type = D_NONE;
		q->from.reg = NREG;
		q->to.reg = rs;
	}
	return q;
}

static Prog *
aword(int32 w, Prog *p)
{
	Prog *q;

	q = prg();
	q->as = AWORD;
	q->line = p->line;
	q->from.type = D_NONE;
	q->reg = NREG;
	q->to.type = D_CONST;
	q->to.offset = w;
	q->link = p->link;
	p->link = q;
	return q;
}

static Prog *
adword(int32 w1, int32 w2, Prog *p)
{
	Prog *q;

	q = prg();
	q->as = ADWORD;
	q->line = p->line;
	q->from.type = D_CONST;
	q->from.offset = w1;
	q->reg = NREG;
	q->to.type = D_CONST;
	q->to.offset = w2;
	q->link = p->link;
	p->link = q;
	return q;
}

void
noops(void)
{
	Prog *p, *q, *q1, *q2;
	int o, foreign;

	/*
	 * find leaf subroutines
	 * strip NOPs
	 * expand RET
	 * expand BECOME pseudo
	 */

#ifdef GOLANG
	Prog *pmorestack;
	Sym *symmorestack;

	symmorestack = lookup("runtime.morestack", 0);
	if(symmorestack->type != STEXT) {
	  diag("runtime·morestack not defined");
	  errorexit();
	} else {
	  pmorestack = symmorestack->text;
	  pmorestack->reg |= NOSPLIT;
	}
#endif

	if(debug['v'])
		Bprint(&bso, "%5.2f noops\n", cputime());
	Bflush(&bso);

	q = P;
	for(cursym = textp; cursym != nil; cursym = cursym->next) {
		for(p = cursym->text; p != P; p = p->link) {
			setarch(p);
	
			switch(p->as) {
			case ATEXT:
				p->mark |= LEAF;
				break;
	
			case ARET:
				break;
	
			case ADIV:
			case ADIVU:
			case AMOD:
			case AMODU:
				q = p;
				if(prog_div == P)
					initdiv();
				cursym->text->mark &= ~LEAF;
				setdiv(p->as);
				continue;
	
			case ANOP:
				q1 = p->link;
				q->link = q1;		/* q is non-nop */
				if(q1 != P)
					q1->mark |= p->mark;
				continue;
	
			case ABL:
			case ABX:
				cursym->text->mark &= ~LEAF;
	
			case ABCASE:
			case AB:
	
			case ABEQ:
			case ABNE:
			case ABCS:
			case ABHS:
			case ABCC:
			case ABLO:
			case ABMI:
			case ABPL:
			case ABVS:
			case ABVC:
			case ABHI:
			case ABLS:
			case ABGE:
			case ABLT:
			case ABGT:
			case ABLE:
				q1 = p->cond;
				if(q1 != P) {
					while(q1->as == ANOP) {
						q1 = q1->link;
						p->cond = q1;
					}
				}
				break;
			}
			q = p;
		}
	}

	for(cursym = textp; cursym != nil; cursym = cursym->next) {
		for(p = cursym->text; p != P; p = p->link) {
			setarch(p);
			o = p->as;
			switch(o) {
			case ATEXT:
				autosize = p->to.offset + 4;
				if(autosize <= 4)
				if(cursym->text->mark & LEAF) {
					p->to.offset = -4;
					autosize = 0;
				}
	
				if(!autosize && !(cursym->text->mark & LEAF)) {
					if(debug['v'])
						Bprint(&bso, "save suppressed in: %s\n",
							cursym->name);
					Bflush(&bso);
					cursym->text->mark |= LEAF;
				}
#ifdef CALLEEBX
				if(p->from.sym->foreign){
					if(thumb)
						// don't allow literal pool to seperate these
						p = adword(0xe28f7001, 0xe12fff17, p); // arm add 1, pc, r7 and bx r7
						// p = aword(0xe12fff17, aword(0xe28f7001, p)); // arm add 1, pc, r7 and bx r7
					else
						p = aword(0x4778, p);	// thumb bx pc and 2 bytes padding
				}
#endif
				if(cursym->text->mark & LEAF) {
					cursym->leaf = 1;
					if(!autosize)
						break;
				}
	
				if(thumb){
                    #ifdef GOLANG
					if(!(p->reg & NOSPLIT))
						diag("stack splitting not supported in thumb");
                    #endif
					if(!(cursym->text->mark & LEAF)){
						q = movrr(nil, REGLINK, REGTMPT-1, p);
						p->link = q;
						q1 = prg();
						q1->as = AMOVW;
						q1->line = p->line;
						q1->from.type = D_REG;
						q1->from.reg = REGTMPT-1;
						q1->to.type = D_OREG;
						q1->to.name = D_NONE;
						q1->to.reg = REGSP;
						q1->to.offset = 0;
						q1->link = q->link;
						q->link = q1;
					}
					if(autosize){
						q2 = prg();
						q2->as = ASUB;
						q2->line = p->line;
						q2->from.type = D_CONST;
						q2->from.offset = autosize;
						q2->to.type = D_REG;
						q2->to.reg = REGSP;
						q2->link = p->link;
						p->link = q2;
					}
					break;
				}
#ifdef GOLANG	
				if((p->reg & NOSPLIT))
#endif
                {
					q1 = prg();
					q1->as = AMOVW;
					q1->scond |= C_WBIT;
					q1->line = p->line;
					q1->from.type = D_REG;
					q1->from.reg = REGLINK;
					q1->to.type = D_OREG;
					q1->to.offset = -autosize;
					q1->to.reg = REGSP;
					q1->link = p->link;
					p->link = q1;
				}
#ifdef GOLANG
                else if (autosize < StackBig) {
					// split stack check for small functions
					// MOVW			g_stackguard(g), R1
					// CMP			R1, $-autosize(SP)
					// MOVW.LO		$autosize, R1
					// MOVW.LO		$args, R2
					// MOVW.LO		R14, R3
					// BL.LO			runtime.morestack(SB) // modifies LR
					// MOVW.W		R14,$-autosize(SP)
	
					// TODO(kaib): add more trampolines
					// TODO(kaib): put stackguard in register
					// TODO(kaib): add support for -K and underflow detection
	
					// MOVW			g_stackguard(g), R1
					p = appendp(p);
					p->as = AMOVW;
					p->from.type = D_OREG;
					p->from.reg = REGG;
					p->to.type = D_REG;
					p->to.reg = 1;
	
					// CMP			R1, $-autosize(SP)
					p = appendp(p);
					p->as = ACMP;
					p->from.type = D_REG;
					p->from.reg = 1;
					p->from.offset = -autosize;
					p->reg = REGSP;
	
					// MOVW.LO		$autosize, R1
					p = appendp(p);
					p->as = AMOVW;
					p->scond = C_SCOND_LO;
					p->from.type = D_CONST;
					p->from.offset = 0;
					p->to.type = D_REG;
					p->to.reg = 1;
	
					// MOVW.LO		$args +4, R2
					// also need to store the extra 4 bytes.
					p = appendp(p);
					p->as = AMOVW;
					p->scond = C_SCOND_LO;
					p->from.type = D_CONST;
					p->from.offset = ((cursym->text->to.offset2 + 3) & ~3) + 4;
					p->to.type = D_REG;
					p->to.reg = 2;
	
					// MOVW.LO	R14, R3
					p = appendp(p);
					p->as = AMOVW;
					p->scond = C_SCOND_LO;
					p->from.type = D_REG;
					p->from.reg = REGLINK;
					p->to.type = D_REG;
					p->to.reg = 3;
	
					// BL.LO		runtime.morestack(SB) // modifies LR
					p = appendp(p);
					p->as = ABL;
					p->scond = C_SCOND_LO;
	 				p->to.type = D_BRANCH;
					p->to.sym = symmorestack;
					p->cond = pmorestack;
	
					// MOVW.W		R14,$-autosize(SP)
					p = appendp(p);
					p->as = AMOVW;
	 				p->scond |= C_WBIT;
					p->from.type = D_REG;
					p->from.reg = REGLINK;
					p->to.type = D_OREG;
					p->to.offset = -autosize;
					p->to.reg = REGSP;
				} else { // > StackBig
					// MOVW		$autosize, R1
					// MOVW		$args, R2
					// MOVW		R14, R3
					// BL			runtime.morestack(SB) // modifies LR
					// MOVW.W		R14,$-autosize(SP)
	
					// MOVW		$autosize, R1
					p = appendp(p);
					p->as = AMOVW;
					p->from.type = D_CONST;
					p->from.offset = autosize;
					p->to.type = D_REG;
					p->to.reg = 1;
	
					// MOVW		$args +4, R2
					// also need to store the extra 4 bytes.
					p = appendp(p);
					p->as = AMOVW;
					p->from.type = D_CONST;
					p->from.offset = ((cursym->text->to.offset2 + 3) & ~3) + 4;
					p->to.type = D_REG;
					p->to.reg = 2;
	
					// MOVW	R14, R3
					p = appendp(p);
					p->as = AMOVW;
					p->from.type = D_REG;
					p->from.reg = REGLINK;
					p->to.type = D_REG;
					p->to.reg = 3;
	
					// BL		runtime.morestack(SB) // modifies LR
					p = appendp(p);
					p->as = ABL;
	 				p->to.type = D_BRANCH;
					p->to.sym = symmorestack;
					p->cond = pmorestack;
	
					// MOVW.W		R14,$-autosize(SP)
					p = appendp(p);
					p->as = AMOVW;
	 				p->scond |= C_WBIT;
					p->from.type = D_REG;
					p->from.reg = REGLINK;
					p->to.type = D_OREG;
					p->to.offset = -autosize;
					p->to.reg = REGSP;
				}
#endif
				break;
	
			case ARET:
				nocache(p);
				foreign = seenthumb && (cursym->foreign || cursym->fnptr);
// print("%s %d %d\n", cursym->name, cursym->foreign, cursym->fnptr);
				if(cursym->text->mark & LEAF) {
					if(!autosize) {
						if(thumb){
							p = fnret(p, REGLINK, foreign, p);
							break;
						}
// if(foreign) print("ABXRET 1 %s\n", cursym->name);
						p->as = foreign ? ABXRET : AB;
						p->from = zprg.from;
						p->to.type = D_OREG;
						p->to.offset = 0;
						p->to.reg = REGLINK;
						break;
					}
				}
				if(thumb){
					if(cursym->text->mark & LEAF){
						if(autosize){
							p->as = AADD;
							p->from.type = D_CONST;
							p->from.offset = autosize;
							p->to.type = D_REG;
							p->to.reg = REGSP;
							q = nil;
						}
						else
							q = p;
						q = fnret(q, REGLINK, foreign, p);
						if(q != p)
							p->link = q;
					}
					else{
						p->as = AMOVW;
						p->from.type = D_OREG;
						p->from.name = D_NONE;
						p->from.reg = REGSP;
						p->from.offset = 0;
						p->to.type = D_REG;
						p->to.reg = REGTMPT-1;
						if(autosize){
							q = prg();
							q->as = AADD;
							q->from.type = D_CONST;
							q->from.offset = autosize;
							q->to.type = D_REG;
							q->to.reg = REGSP;
							q->link = p->link;
							p->link = 	q;
						}
						else
							q = p;
						q1 = fnret(nil, REGTMPT-1, foreign, p);
						q1->link = q->link;
						q->link = q1;
					}
					break;
				}
				if(foreign) {
// if(foreign) print("ABXRET 3 %s\n", cursym->name);
#define	R	1
					p->as = AMOVW;
					p->from.type = D_OREG;
					p->from.name = D_NONE;
					p->from.reg = REGSP;
					p->from.offset = 0;
					p->to.type = D_REG;
					p->to.reg = R;
					q = prg();
					q->as = AADD;
					q->scond = p->scond;
					q->line = p->line;
					q->from.type = D_CONST;
					q->from.offset = autosize;
					q->to.type = D_REG;
					q->to.reg = REGSP;
					q->link = p->link;
					p->link = q;
					q1 = prg();
					q1->as = ABXRET;
					q1->scond = p->scond;
					q1->line = p->line;
					q1->to.type = D_OREG;
					q1->to.offset = 0;
					q1->to.reg = R;
					q1->link = q->link;
					q->link = q1;
#undef	R
				}
				else {
					p->as = AMOVW;
					p->scond |= C_PBIT;
					p->from.type = D_OREG;
					p->from.offset = autosize;
					p->from.reg = REGSP;
					p->to.type = D_REG;
					p->to.reg = REGPC;
				}
				break;
	
			case ADIV:
			case ADIVU:
			case AMOD:
			case AMODU:
				if(debug['M'])
					break;
				if(p->from.type != D_REG)
					break;
				if(p->to.type != D_REG)
					break;
				q1 = p;
	
				/* MOV a,4(SP) */
				q = prg();
				q->link = p->link;
				p->link = q;
				p = q;
	
				p->as = AMOVW;
				p->line = q1->line;
				p->from.type = D_REG;
				p->from.reg = q1->from.reg;
				p->to.type = D_OREG;
				p->to.reg = REGSP;
				p->to.offset = 4;
	
				/* MOV b,REGTMP */
				q = prg();
				q->link = p->link;
				p->link = q;
				p = q;
	
				p->as = AMOVW;
				p->line = q1->line;
				p->from.type = D_REG;
				p->from.reg = q1->reg;
				if(q1->reg == NREG)
					p->from.reg = q1->to.reg;
				p->to.type = D_REG;
				p->to.reg = prog_div->from.sym->thumb ? REGTMPT : REGTMP;
				p->to.offset = 0;
	
				/* CALL appropriate */
				q = prg();
				q->link = p->link;
				p->link = q;
				p = q;
	
#ifdef CALLEEBX
				p->as = ABL;
#else
				if(prog_div->from.sym->thumb)
					p->as = thumb ? ABL : ABX;
				else
					p->as = thumb ? ABX : ABL;
#endif
				p->line = q1->line;
				p->to.type = D_BRANCH;
				p->cond = p;
				switch(o) {
				case ADIV:
					p->cond = prog_div;
					p->to.sym = sym_div;
					break;
				case ADIVU:
					p->cond = prog_divu;
					p->to.sym = sym_divu;
					break;
				case AMOD:
					p->cond = prog_mod;
					p->to.sym = sym_mod;
					break;
				case AMODU:
					p->cond = prog_modu;
					p->to.sym = sym_modu;
					break;
				}
	
				/* MOV REGTMP, b */
				q = prg();
				q->link = p->link;
				p->link = q;
				p = q;
	
				p->as = AMOVW;
				p->line = q1->line;
				p->from.type = D_REG;
				p->from.reg = prog_div->from.sym->thumb ? REGTMPT : REGTMP;
				p->from.offset = 0;
				p->to.type = D_REG;
				p->to.reg = q1->to.reg;
	
				/* ADD $8,SP */
				q = prg();
				q->link = p->link;
				p->link = q;
				p = q;
	
				p->as = AADD;
				p->from.type = D_CONST;
				p->from.reg = NREG;
				p->from.offset = 8;
				p->reg = NREG;
				p->to.type = D_REG;
				p->to.reg = REGSP;
	
				/* SUB $8,SP */
				q1->as = ASUB;
				q1->from.type = D_CONST;
				q1->from.offset = 8;
				q1->from.reg = NREG;
				q1->reg = NREG;
				q1->to.type = D_REG;
				q1->to.reg = REGSP;
	
				break;
			case AMOVW:
				if(thumb){
					Adr *a = &p->from;
	
					if(a->type == D_CONST && ((a->name == D_NONE && a->reg == REGSP) || a->name == D_AUTO || a->name == D_PARAM) && (a->offset & 3))
						diag("SP offset not multiple of 4");
				}
				break;
			case AMOVB:
			case AMOVBU:
			case AMOVH:
			case AMOVHU:
				if(thumb){
					if(p->from.type == D_OREG && (p->from.name == D_AUTO || p->from.name == D_PARAM || (p->from.name == D_CONST && p->from.reg == REGSP))){
						q = prg();
						*q = *p;
						if(p->from.name == D_AUTO)
							q->from.offset += autosize;
						else if(p->from.name == D_PARAM)
							q->from.offset += autosize+4;
						q->from.name = D_NONE;
						q->from.reg = REGTMPT;
						p = movrr(p, REGSP, REGTMPT, p);
						q->link = p->link;
						p->link = q;
					}
					if(p->to.type == D_OREG && (p->to.name == D_AUTO || p->to.name == D_PARAM || (p->to.name == D_CONST && p->to.reg == REGSP))){
						q = prg();
						*q = *p;
						if(p->to.name == D_AUTO)
							q->to.offset += autosize;
						else if(p->to.name == D_PARAM)
							q->to.offset += autosize+4;
						q->to.name = D_NONE;
						q->to.reg = REGTMPT;
						p = movrr(p, REGSP, REGTMPT, p);
						q->link = p->link;
						p->link = q;
						if(q->to.offset < 0 || q->to.offset > 255){	// complicated
							p->to.reg = REGTMPT+1;			// mov sp, r8
							q1 = prg();
							q1->line = p->line;
							q1->as = AMOVW;
							q1->from.type = D_CONST;
							q1->from.offset = q->to.offset;
							q1->to.type = D_REG;
							q1->to.reg = REGTMPT;			// mov $o, r7
							p->link = q1;
							q1->link = q;
							q1 = prg();
							q1->line = p->line;
							q1->as = AADD;
							q1->from.type = D_REG;
							q1->from.reg = REGTMPT+1;
							q1->to.type = D_REG;
							q1->to.reg = REGTMPT;			// add r8, r7
							p->link->link = q1;
							q1->link = q;
							q->to.offset = 0;				// mov* r, 0(r7)
							/* phew */
						}
					}
				}
				break;
			case AMOVM:
				if(thumb){
					if(p->from.type == D_OREG){
						if(p->from.offset == 0)
							p->from.type = D_REG;
						else
							diag("non-zero AMOVM offset");
					}
					else if(p->to.type == D_OREG){
						if(p->to.offset == 0)
							p->to.type = D_REG;
						else
							diag("non-zero AMOVM offset");
					}
				}
				break;
			case AB:
				if(thumb && p->to.type == D_OREG){
					if(p->to.offset == 0){
						p->as = AMOVW;
						p->from.type = D_REG;
						p->from.reg = p->to.reg;
						p->to.type = D_REG;
						p->to.reg = REGPC;
					}
					else{
						p->as = AADD;
						p->from.type = D_CONST;
						p->from.offset = p->to.offset;
						p->reg = p->to.reg;
						p->to.type = D_REG;
						p->to.reg = REGTMPT-1;
						q = prg();
						q->as = AMOVW;
						q->line = p->line;
						q->from.type = D_REG;
						q->from.reg = REGTMPT-1;
						q->to.type = D_REG;
						q->to.reg = REGPC;
						q->link = p->link;
						p->link = q;
					}
				}
				if(seenthumb && !thumb && p->to.type == D_OREG && p->to.reg == REGLINK){
					// print("warn %s:	b	(R%d)	assuming a return\n", cursym->name, p->to.reg);
					p->as = ABXRET;
				}
				break;
			case ABL:
			case ABX:
				if(thumb && p->to.type == D_OREG){
					if(p->to.offset == 0){
						p->as = o;
						p->from.type = D_NONE;
						p->to.type = D_REG;
					}
					else{
						p->as = AADD;
						p->from.type = D_CONST;
						p->from.offset = p->to.offset;
						p->reg = p->to.reg;
						p->to.type = D_REG;
						p->to.reg = REGTMPT-1;
						q = prg();
						q->as = o;
						q->line = p->line;
						q->from.type = D_NONE;
						q->to.type = D_REG;
						q->to.reg = REGTMPT-1;
						q->link = p->link;
						p->link = q;
					}
				}
				break;
			}
		}
	}
}

static void
sigdiv(char *n)
{
	Sym *s;

	s = lookup(n, 0);
	if(s->type == STEXT)
		if(s->sig == 0)
			s->sig = SIGNINTERN;
}

void
divsig(void)
{
	sigdiv("_div");
	sigdiv("_divu");
	sigdiv("_mod");
	sigdiv("_modu");
}

void
initdiv(void)
{
	Sym *s2, *s3, *s4, *s5;

	if(prog_div != P)
		return;
	sym_div = s2 = lookup("_div", 0);
	sym_divu = s3 = lookup("_divu", 0);
	sym_mod = s4 = lookup("_mod", 0);
	sym_modu = s5 = lookup("_modu", 0);
	prog_div = s2->text;
	prog_divu = s3->text;
	prog_mod = s4->text;
	prog_modu = s5->text;
	if(prog_div == P) {
		diag("undefined: %s", s2->name);
		prog_div = cursym->text;
	}
	if(prog_divu == P) {
		diag("undefined: %s", s3->name);
		prog_divu = cursym->text;
	}
	if(prog_mod == P) {
		diag("undefined: %s", s4->name);
		prog_mod = cursym->text;
	}
	if(prog_modu == P) {
		diag("undefined: %s", s5->name);
		prog_modu = cursym->text;
	}
}

static void
setdiv(int as)
{
	Prog *p = nil;

	switch(as){
	case ADIV: p = prog_div; break;
	case ADIVU: p = prog_divu; break;
	case AMOD: p = prog_mod; break;
	case AMODU: p = prog_modu; break;
	}
	if(thumb != p->from.sym->thumb)
		p->from.sym->foreign = 1;
}

void
nocache(Prog *p)
{
	p->optab = 0;
	p->from.class = 0;
	p->to.class = 0;
}
