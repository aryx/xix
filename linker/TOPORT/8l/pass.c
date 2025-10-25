// Inferno utils/8l/pass.c
// http://code.google.com/p/inferno-os/source/browse/utils/8l/pass.c
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

// Code and data passes.

#include	"l.h"
#include	"../ld/lib.h"

static void xfol(Prog*, Prog**);

// see ../../pkg/runtime/proc.c:/StackGuard
enum
{
	StackSmall = 128,
	StackBig = 4096,
};

Prog*
brchain(Prog *p)
{
	int i;

	for(i=0; i<20; i++) {
		if(p == P || p->as != AJMP)
			return p;
		p = p->pcond;
	}
	return P;
}

void
follow(void)
{
	Prog *firstp, *lastp;

	if(debug['v'])
		Bprint(&bso, "%5.2f follow\n", cputime());
	Bflush(&bso);
	
	for(cursym = textp; cursym != nil; cursym = cursym->next) {
		firstp = prg();
		lastp = firstp;
		xfol(cursym->text, &lastp);
		lastp->link = nil;
		cursym->text = firstp->link;
	}
}

static int
nofollow(int a)
{
	switch(a) {
	case AJMP:
	case ARET:
	case AIRETL:
	case AIRETW:
		return 1;
	}
	return 0;
}

static int
pushpop(int a)
{
	switch(a) {
	case APUSHL:
	case APUSHFL:
	case APUSHW:
	case APUSHFW:
	case APOPL:
	case APOPFL:
	case APOPW:
	case APOPFW:
		return 1;
	}
	return 0;
}

static void
xfol(Prog *p, Prog **last)
{
	Prog *q;
	int i;
	enum as a;

loop:
	if(p == P)
		return;
	if(p->as == AJMP)
	if((q = p->pcond) != P && q->as != ATEXT) {
		/* mark instruction as done and continue layout at target of jump */
		p->mark = 1;
		p = q;
		if(p->mark == 0)
			goto loop;
	}
	if(p->mark) {
		/* 
		 * p goes here, but already used it elsewhere.
		 * copy up to 4 instructions or else branch to other copy.
		 */
		for(i=0,q=p; i<4; i++,q=q->link) {
			if(q == P)
				break;
			if(q == *last)
				break;
			a = q->as;
			if(a == ANOP) {
				i--;
				continue;
			}
			if(nofollow(a) || pushpop(a))	
				break;	// NOTE(rsc): arm does goto copy
			if(q->pcond == P || q->pcond->mark)
				continue;
			if(a == ACALL || a == ALOOP)
				continue;
			for(;;) {
				if(p->as == ANOP) {
					p = p->link;
					continue;
				}
				q = copyp(p);
				p = p->link;
				q->mark = 1;
				(*last)->link = q;
				*last = q;
				if(q->as != a || q->pcond == P || q->pcond->mark)
					continue;

				q->as = relinv(q->as);
				p = q->pcond;
				q->pcond = q->link;
				q->link = p;
				xfol(q->link, last);
				p = q->link;
				if(p->mark)
					return;
				goto loop;
			}
		} /* */
		q = prg();
		q->as = AJMP;
		q->line = p->line;
		q->to.type = D_BRANCH;
		q->to.offset = p->pc;
		q->pcond = p;
		p = q;
	}
	
	/* emit p */
	p->mark = 1;
	(*last)->link = p;
	*last = p;
	a = p->as;

	/* continue loop with what comes after p */
	if(nofollow(a))
		return;
	if(p->pcond != P && a != ACALL) {
		/*
		 * some kind of conditional branch.
		 * recurse to follow one path.
		 * continue loop on the other.
		 */
		q = brchain(p->link);
		if(q != P && q->mark)
		if(a != ALOOP) {
			p->as = relinv(a);
			p->link = p->pcond;
			p->pcond = q;
		}
		xfol(p->link, last);
		q = brchain(p->pcond);
		if(q->mark) {
			p->pcond = q;
			return;
		}
		p = q;
		goto loop;
	}
	p = p->link;
	goto loop;
}

int
relinv(int a)
{

	switch(a) {
	case AJEQ:	return AJNE;
	case AJNE:	return AJEQ;
	case AJLE:	return AJGT;
	case AJLS:	return AJHI;
	case AJLT:	return AJGE;
	case AJMI:	return AJPL;
	case AJGE:	return AJLT;
	case AJPL:	return AJMI;
	case AJGT:	return AJLE;
	case AJHI:	return AJLS;
	case AJCS:	return AJCC;
	case AJCC:	return AJCS;
	case AJPS:	return AJPC;
	case AJPC:	return AJPS;
	case AJOS:	return AJOC;
	case AJOC:	return AJOS;
	}
	diag("unknown relation: %s in %s", anames[a], TNAME);
	return a;
}

void
patch(void)
{
	int32 c;
	Prog *p, *q;
	Sym *s;
	int32 vexit;

	if(debug['v'])
		Bprint(&bso, "%5.2f mkfwd\n", cputime());
	Bflush(&bso);
	mkfwd();
	if(debug['v'])
		Bprint(&bso, "%5.2f patch\n", cputime());
	Bflush(&bso);
	s = lookup("exit", 0);
	vexit = s->value;
	for(cursym = textp; cursym != nil; cursym = cursym->next) {
		for(p = cursym->text; p != P; p = p->link) {
			if(HEADTYPE == 10) {	// Windows
				// Convert
				//   op	  n(GS), reg
				// to
				//   MOVL 0x2C(FS), reg
				//   op	  n(reg), reg
				// The purpose of this patch is to fix some accesses
				// to extern register variables (TLS) on Windows, as
				// a different method is used to access them.
				if(p->from.type == D_INDIR+D_GS
				&& p->to.type >= D_AX && p->to.type <= D_DI) {
					q = appendp(p);
					q->from = p->from;
					q->from.type = D_INDIR + p->to.type;
					q->to = p->to;
					q->as = p->as;
					p->as = AMOVL;
					p->from.type = D_INDIR+D_FS;
					p->from.offset = 0x2C;
				}
			}
			if(HEADTYPE == 7) {	// Linux
				// Running binaries under Xen requires using
				//	MOVL 0(GS), reg
				// and then off(reg) instead of saying off(GS) directly
				// when the offset is negative.
				if(p->from.type == D_INDIR+D_GS && p->from.offset < 0
				&& p->to.type >= D_AX && p->to.type <= D_DI) {
					q = appendp(p);
					q->from = p->from;
					q->from.type = D_INDIR + p->to.type;
					q->to = p->to;
					q->as = p->as;
					p->as = AMOVL;
					p->from.type = D_INDIR+D_GS;
					p->from.offset = 0;
				}
			}
			if(HEADTYPE == 2) {	// Plan 9
				if(p->from.type == D_INDIR+D_GS
				&& p->to.type >= D_AX && p->to.type <= D_DI) {
					p->as = AMOVL;
					p->from.type = D_ADDR+D_STATIC;
					p->from.offset += 0xdfffefc0;
				}
			}
			if(p->as == ACALL || (p->as == AJMP && p->to.type != D_BRANCH)) {
				s = p->to.sym;
				if(s) {
					if(debug['c'])
						Bprint(&bso, "%s calls %s\n", TNAME, s->name);
					switch(s->type) {
					default:
						/* diag prints TNAME first */
						diag("undefined: %s", s->name);
						s->type = STEXT;
						s->value = vexit;
						continue;	// avoid more error messages
					case STEXT:
						p->to.offset = s->value;
						break;
					}
					p->to.type = D_BRANCH;
				}
			}
			if(p->to.type != D_BRANCH)
				continue;
			c = p->to.offset;
			for(q = textp->text; q != P;) {
				if(c == q->pc)
					break;
				if(q->forwd != P && c >= q->forwd->pc)
					q = q->forwd;
				else
					q = q->link;
			}
			if(q == P) {
				diag("branch out of range in %s\n%P", TNAME, p);
				p->to.type = D_NONE;
			}
			p->pcond = q;
		}
	}

	for(cursym = textp; cursym != nil; cursym = cursym->next) {
		for(p = cursym->text; p != P; p = p->link) {
			p->mark = 0;	/* initialization for follow */
			if(p->pcond != P) {
				p->pcond = brloop(p->pcond);
				if(p->pcond != P)
				if(p->to.type == D_BRANCH)
					p->to.offset = p->pcond->pc;
			}
		}
	}
}

Prog*
brloop(Prog *p)
{
	int c;
	Prog *q;

	c = 0;
	for(q = p; q != P; q = q->pcond) {
		if(q->as != AJMP)
			break;
		c++;
		if(c >= 5000)
			return P;
	}
	return q;
}

void
dostkoff(void)
{
	Prog *p, *q, *q1;
	int32 autoffset, deltasp;
	int a;
#ifdef GOLANG
	Prog *pmorestack;
	Sym *symmorestack;

	pmorestack = P;
	symmorestack = lookup("runtime.morestack", 0);

	if(symmorestack->type != STEXT)
	  diag("runtime.morestack not defined");
	else {
	  pmorestack = symmorestack->text;
	  symmorestack->text->from.scale |= NOSPLIT;
	}
#endif

	for(cursym = textp; cursym != nil; cursym = cursym->next) {
		p = cursym->text;

		autoffset = p->to.offset;
		if(autoffset < 0)
			autoffset = 0;

		q = P;
		q1 = P;
#ifdef GOLANG
		if(pmorestack != P)
		if(!(p->from.scale & NOSPLIT)) {
			p = appendp(p);	// load g into CX
			switch(HEADTYPE) {
			case 10:	// Windows
				p->as = AMOVL;
				p->from.type = D_INDIR+D_FS;
				p->from.offset = 0x2c;
				p->to.type = D_CX;

				p = appendp(p);
				p->as = AMOVL;
				p->from.type = D_INDIR+D_CX;
				p->from.offset = 0;
				p->to.type = D_CX;
				break;
			
			case 7:	// Linux
				p->as = AMOVL;
				p->from.type = D_INDIR+D_GS;
				p->from.offset = 0;
				p->to.type = D_CX;

				p = appendp(p);
				p->as = AMOVL;
				p->from.type = D_INDIR+D_CX;
				p->from.offset = tlsoffset + 0;
				p->to.type = D_CX;
				break;
			
			case 2:	// Plan 9
				p->as = AMOVL;
				p->from.type = D_ADDR+D_STATIC;
				p->from.offset = 0xdfffefc0;
				p->to.type = D_CX;
				break;
			
			default:
				p->as = AMOVL;
				p->from.type = D_INDIR+D_GS;
				p->from.offset = tlsoffset + 0;
				p->to.type = D_CX;
			}

			if(debug['K']) {
				// 8l -K means check not only for stack
				// overflow but stack underflow.
				// On underflow, INT 3 (breakpoint).
				// Underflow itself is rare but this also
				// catches out-of-sync stack guard info.
				p = appendp(p);
				p->as = ACMPL;
				p->from.type = D_INDIR+D_CX;
				p->from.offset = 4;
				p->to.type = D_SP;

				p = appendp(p);
				p->as = AJCC;
				p->to.type = D_BRANCH;
				p->to.offset = 4;
				q1 = p;

				p = appendp(p);
				p->as = AINT;
				p->from.type = D_CONST;
				p->from.offset = 3;
				
				p = appendp(p);
				p->as = ANOP;
				q1->pcond = p;
			}

			if(autoffset < StackBig) {  // do we need to call morestack
				if(autoffset <= StackSmall) {
					// small stack
					p = appendp(p);
					p->as = ACMPL;
					p->from.type = D_SP;
					p->to.type = D_INDIR+D_CX;
				} else {
					// large stack
					p = appendp(p);
					p->as = ALEAL;
					p->from.type = D_INDIR+D_SP;
					p->from.offset = -(autoffset-StackSmall);
					p->to.type = D_AX;

					p = appendp(p);
					p->as = ACMPL;
					p->from.type = D_AX;
					p->to.type = D_INDIR+D_CX;
				}

				// common
				p = appendp(p);
				p->as = AJHI;
				p->to.type = D_BRANCH;
				p->to.offset = 4;
				q = p;
			}

			p = appendp(p);	// save frame size in DX
			p->as = AMOVL;
			p->to.type = D_DX;
			/* 160 comes from 3 calls (3*8) 4 safes (4*8) and 104 guard */
			p->from.type = D_CONST;
			if(autoffset+160 > 4096)
				p->from.offset = (autoffset+160) & ~7LL;

			p = appendp(p);	// save arg size in AX
			p->as = AMOVL;
			p->to.type = D_AX;
			p->from.type = D_CONST;
			p->from.offset = cursym->text->to.offset2;

			p = appendp(p);
			p->as = ACALL;
			p->to.type = D_BRANCH;
			p->pcond = pmorestack;
			p->to.sym = symmorestack;

		}
#endif

		if(q != P)
			q->pcond = p->link;

		if(autoffset) {
			p = appendp(p);
			p->as = AADJSP;
			p->from.type = D_CONST;
			p->from.offset = autoffset;
			p->spadj = autoffset;
			if(q != P)
				q->pcond = p;
		}
		deltasp = autoffset;
		
		for(; p != P; p = p->link) {
			a = p->from.type;
			if(a == D_AUTO)
				p->from.offset += deltasp;
			if(a == D_PARAM)
				p->from.offset += deltasp + 4;
			a = p->to.type;
			if(a == D_AUTO)
				p->to.offset += deltasp;
			if(a == D_PARAM)
				p->to.offset += deltasp + 4;
	
			switch(p->as) {
			default:
				continue;
			case APUSHL:
			case APUSHFL:
				deltasp += 4;
				p->spadj = 4;
				continue;
			case APUSHW:
			case APUSHFW:
				deltasp += 2;
				p->spadj = 2;
				continue;
			case APOPL:
			case APOPFL:
				deltasp -= 4;
				p->spadj = -4;
				continue;
			case APOPW:
			case APOPFW:
				deltasp -= 2;
				p->spadj = -2;
				continue;
			case ARET:
				break;
			}
	
			if(autoffset != deltasp)
				diag("unbalanced PUSH/POP");
	
			if(autoffset) {
				q = p;
				p = appendp(p);
				p->as = ARET;
	
				q->as = AADJSP;
				q->from.type = D_CONST;
				q->from.offset = -autoffset;
				p->spadj = -autoffset;
			}
		}
	}
}

int32
atolwhex(char *s)
{
	int32 n;
	int f;

	n = 0;
	f = 0;
	while(*s == ' ' || *s == '\t')
		s++;
	if(*s == '-' || *s == '+') {
		if(*s++ == '-')
			f = 1;
		while(*s == ' ' || *s == '\t')
			s++;
	}
	if(s[0]=='0' && s[1]){
		if(s[1]=='x' || s[1]=='X'){
			s += 2;
			for(;;){
				if(*s >= '0' && *s <= '9')
					n = n*16 + *s++ - '0';
				else if(*s >= 'a' && *s <= 'f')
					n = n*16 + *s++ - 'a' + 10;
				else if(*s >= 'A' && *s <= 'F')
					n = n*16 + *s++ - 'A' + 10;
				else
					break;
			}
		} else
			while(*s >= '0' && *s <= '7')
				n = n*8 + *s++ - '0';
	} else
		while(*s >= '0' && *s <= '9')
			n = n*10 + *s++ - '0';
	if(f)
		n = -n;
	return n;
}

void
undef(void)
{
	int i;
	Sym *s;

	for(i=0; i<NHASH; i++)
	for(s = hash[i]; s != S; s = s->hash)
		if(s->type == SXREF)
			diag("%s: not defined", s->name);
}
