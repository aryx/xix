// Inferno utils/6l/obj.c
// http://code.google.com/p/inferno-os/source/browse/utils/6l/obj.c
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

// Reading object files.

#define	EXTERN
#include	"l.h"

#include	"../ld/lib.h"
#include	"../ld/elf_.h"
#include	"../ld/macho.h"
#include	"../ld/dwarf.h"

#include	<ar.h>

char	*noname		= "<none>";
char	thechar		= '6';
char*	thestring 	= "amd64";

// ??
char*	paramspace	= "FP";

/*
 *	-H2 -T4136 -R4096		is plan9 64-bit format
 *	-H3 -T4128 -R4096		is plan9 32-bit format
 // TODO: reverse -H2/-H3 to be consistent, use -H2 for plan9 32 bit?
 *	-H5 -T0x80110000 -R4096		is ELF32
 *	-H6 -Tx -Rx			is apple MH-exec
 *	-H7 -Tx -Rx			is linux elf-exec
 *  -H9 -Tx -Rx			is FreeBSD elf-exec
 *
 *	options used: 189BLQSWabcjlnpsvz
 */

void
usage(void)
{
	fprint(2, "usage: 6l [-options] [-E entry] [-H head] [-L dir] [-T text] [-R rnd] [-r path] [-o out] main.6\n");
	exits("usage");
}

void
main(int argc, char *argv[])
{
	int c;

	Binit(&bso, 1, OWRITE);
	cout = -1;
	listinit();
	memset(debug, 0, sizeof(debug));
	nerrors = 0;
	outfile = "6.out";
	HEADTYPE = -1;
	INITTEXT = -1;
	INITDAT = -1;
	INITRND = -1;
	INITENTRY = 0;

	ARGBEGIN {
	default:
		c = ARGC();
		if(c == 'l')
			usage();
 		if(c >= 0 && c < sizeof(debug))
			debug[c]++;
		break;
	case 'o': /* output to (next arg) */
		outfile = EARGF(usage());
		break;
	case 'E':
		INITENTRY = EARGF(usage());
		break;
	case 'H':
		HEADTYPE = atolwhex(EARGF(usage()));
		break;
	case 'L':
		Lflag(EARGF(usage()));
		break;
	case 'T':
		INITTEXT = atolwhex(EARGF(usage()));
		break;
	case 'D':
		INITDAT = atolwhex(EARGF(usage()));
		break;
	case 'R':
		INITRND = atolwhex(EARGF(usage()));
		break;
#ifdef GOLANG
	case 'r':
		rpath = EARGF(usage());
		break;
#endif
	case 'V':
		print("%cl version %s\n", thechar, getgoversion());
		errorexit();
	} ARGEND

	if(argc != 1)
		usage();

	libinit();
#ifdef GOLANG
	if(rpath == nil)
		rpath = smprint("%s/pkg/%s_%s", goroot, goos, goarch);
#endif

	if(HEADTYPE == -1) {
		HEADTYPE = 2;
		if(strcmp(goos, "linux") == 0)
			HEADTYPE = 7;
		else
		if(strcmp(goos, "darwin") == 0)
			HEADTYPE = 6;
		else
		if(strcmp(goos, "freebsd") == 0)
			HEADTYPE = 9;
		else
			print("goos is not known: %s\n", goos);
	}

	switch(HEADTYPE) {
	case 2:	/* plan 9 */
		HEADR = 32L+8L;
		if(INITTEXT == -1)
			INITTEXT = 4096+HEADR;
		if(INITDAT == -1)
			INITDAT = 0;
		if(INITRND == -1)
			INITRND = 4096;
		break;
	case 3:	/* plan 9 */
		HEADR = 32L;
		if(INITTEXT == -1)
			INITTEXT = 4096+32;
		if(INITDAT == -1)
			INITDAT = 0;
		if(INITRND == -1)
			INITRND = 4096;
		break;
	case 5:	/* elf32 executable */
		HEADR = rnd(52L+3*32L, 16);
		if(INITTEXT == -1)
			INITTEXT = 0x80110000L;
		if(INITDAT == -1)
			INITDAT = 0;
		if(INITRND == -1)
			INITRND = 4096;
		break;
	case 6:	/* apple MACH */
		/*
		 * OS X system constant - offset from 0(GS) to our TLS.
		 * Explained in ../../libcgo/darwin_amd64.c.
		 */
		tlsoffset = 0x8a0;
		machoinit();
		HEADR = MACHORESERVE;
		if(INITRND == -1)
			INITRND = 4096;
		if(INITTEXT == -1)
			INITTEXT = 4096+HEADR;
		if(INITDAT == -1)
			INITDAT = 0;
		break;
	case 7:	/* elf64 executable */
	case 9: /* freebsd */
		/*
		 * ELF uses TLS offset negative from FS.
		 * Translate 0(FS) and 8(FS) into -16(FS) and -8(FS).
		 * Also known to ../../pkg/runtime/linux/amd64/sys.s
		 * and ../../libcgo/linux_amd64.s.
		 */
		tlsoffset = -16;
		elfinit();
		HEADR = ELFRESERVE;
		if(INITTEXT == -1)
			INITTEXT = (1<<22)+HEADR;
		if(INITDAT == -1)
			INITDAT = 0;
		if(INITRND == -1)
			INITRND = 4096;
		break;
	default:
		diag("unknown -H option");
		errorexit();
	}
	if(INITDAT != 0 && INITRND != 0)
		print("warning: -D0x%llux is ignored because of -R0x%ux\n",
			INITDAT, INITRND);
	if(debug['v'])
		Bprint(&bso, "HEADER = -H%d -T0x%llux -D0x%llux -R0x%ux\n",
			HEADTYPE, INITTEXT, INITDAT, INITRND);
	Bflush(&bso);
	instinit();

	zprg.link = P;
	zprg.pcond = P;
	zprg.back = 2;
	zprg.as = AGOK;
	zprg.from.type = D_NONE;
	zprg.from.index = D_NONE;
	zprg.from.scale = 1;
	zprg.to = zprg.from;
	zprg.mode = 64;

	pcstr = "%.6llux ";
	nuxiinit();
	histgen = 0;
	pc = 0;
	dtype = 4;
	version = 0;
	cbp = buf.cbuf;
	cbc = sizeof(buf.cbuf);

	addlibpath("command line", "command line", argv[0], "main");
	loadlib();
	deadcode();
	patch();
	follow();
	doelf();
	if(HEADTYPE == 6)
		domacho();
	dodata();
	dostkoff();
	paramspace = "SP";	/* (FP) now (SP) on output */
	if(debug['p'])
		if(debug['1'])
			doprof1();
		else
			doprof2();
	span();
	reloc();
	asmb();
	undef();
	if(debug['v']) {
		Bprint(&bso, "%5.2f cpu time\n", cputime());
		Bprint(&bso, "%d symbols\n", nsymbol);
		Bprint(&bso, "%d sizeof adr\n", sizeof(Adr));
		Bprint(&bso, "%d sizeof prog\n", sizeof(Prog));
	}
	Bflush(&bso);

	errorexit();
}

static Sym*
zsym(char *pn, Biobuf *f, Sym *h[])
{	
	int o;
	
	o = Bgetc(f);
	if(o < 0 || o >= NSYM || h[o] == nil)
		mangle(pn);
	return h[o];
}

static void
zaddr(char *pn, Biobuf *f, Adr *a, Sym *h[])
{
	int t;
	int32 l;
	Sym *s;
	Auto *u;

	t = Bgetc(f);
	a->index = D_NONE;
	a->scale = 0;
	if(t & T_INDEX) {
		a->index = Bgetc(f);
		a->scale = Bgetc(f);
	}
	a->offset = 0;
	if(t & T_OFFSET) {
		a->offset = Bget4(f);
		if(t & T_64) {
			a->offset &= 0xFFFFFFFFULL;
			a->offset |= (vlong)Bget4(f) << 32;
		}
	}
	a->sym = S;
	if(t & T_SYM)
		a->sym = zsym(pn, f, h);
	a->type = D_NONE;
	if(t & T_FCONST) {
		a->ieee.l = Bget4(f);
		a->ieee.h = Bget4(f);
		a->type = D_FCONST;
	} else
	if(t & T_SCONST) {
		Bread(f, a->scon, NSNAME);
		a->type = D_SCONST;
	}
	if(t & T_TYPE)
		a->type = Bgetc(f);
	if(a->type < 0 || a->type >= D_SIZE)
		mangle(pn);
	adrgotype = S;
	if(t & T_GOTYPE)
		adrgotype = zsym(pn, f, h);
	s = a->sym;
	t = a->type;
	if(t == D_INDIR+D_GS)
		a->offset += tlsoffset;
	if(t != D_AUTO && t != D_PARAM) {
		if(s && adrgotype)
			s->gotype = adrgotype;
		return;
	}
	l = a->offset;
	for(u=curauto; u; u=u->link) {
		if(u->asym == s)
		if(u->type == t) {
			if(u->aoffset > l)
				u->aoffset = l;
			if(adrgotype)
				u->gotype = adrgotype;
			return;
		}
	}

	u = mal(sizeof(*u));
	u->link = curauto;
	curauto = u;
	u->asym = s;
	u->aoffset = l;
	u->type = t;
	u->gotype = adrgotype;
}

void
nopout(Prog *p)
{
	p->as = ANOP;
	p->from.type = D_NONE;
	p->to.type = D_NONE;
}

void
ldobj1(Biobuf *f, char *pkg, int64 len, char *pn)
{
	vlong ipc;
	Prog *p;
	int v, o, r, skip, mode;
	Sym *h[NSYM], *s, *di;
	uint32 sig;
	char *name, *x;
	int ntext;
	vlong eof;
	char src[1024];
	Prog *lastp;

	lastp = nil;
	ntext = 0;
	eof = Boffset(f) + len;
	di = S;
	src[0] = 0;

newloop:
	memset(h, 0, sizeof(h));
	version++;
	histfrogp = 0;
	ipc = pc;
	skip = 0;
	mode = 64;

loop:
	if(f->state == Bracteof || Boffset(f) >= eof)
		goto eof;
	o = Bgetc(f);
	if(o == Beof)
		goto eof;
	o |= Bgetc(f) << 8;
	if(o <= AXXX || o >= ALAST) {
		if(o < 0)
			goto eof;
		diag("%s:#%lld: opcode out of range: %#ux", pn, Boffset(f), o);
		print("	probably not a .6 file\n");
		errorexit();
	}

	if(o == ANAME || o == ASIGNAME) {
		sig = 0;
		if(o == ASIGNAME)
			sig = Bget4(f);
		v = Bgetc(f);	/* type */
		o = Bgetc(f);	/* sym */
		r = 0;
		if(v == D_STATIC)
			r = version;
		name = Brdline(f, '\0');
		if(name == nil) {
			if(Blinelen(f) > 0) {
				fprint(2, "%s: name too long\n", pn);
				errorexit();
			}
			goto eof;
		}
		x = expandpkg(name, pkg);
		s = lookup(x, r);
		if(x != name)
			free(x);
		name = nil;

		if(debug['S'] && r == 0)
			sig = 1729;
		if(sig != 0){
			if(s->sig != 0 && s->sig != sig)
				diag("incompatible type signatures"
					"%ux(%s) and %ux(%s) for %s",
					s->sig, s->file, sig, pn, s->name);
			s->sig = sig;
			s->file = pn;
		}

		if(debug['W'])
			print("	ANAME	%s\n", s->name);
		if(o < 0 || o >= nelem(h))
			mangle(pn);
		h[o] = s;
		if((v == D_EXTERN || v == D_STATIC) && s->type == 0)
			s->type = SXREF;
		if(v == D_FILE) {
			if(s->type != SFILE) {
				histgen++;
				s->type = SFILE;
				s->value = histgen;
			}
			if(histfrogp < MAXHIST) {
				histfrog[histfrogp] = s;
				histfrogp++;
			} else
				collapsefrog(s);
			dwarfaddfrag(s->value, s->name);
		}
		goto loop;
	}

	p = mal(sizeof(*p));
	p->as = o;
	p->line = Bget4(f);
	p->back = 2;
	p->mode = mode;
	p->ft = 0;
	p->tt = 0;
	zaddr(pn, f, &p->from, h);
	fromgotype = adrgotype;
	zaddr(pn, f, &p->to, h);
	
	switch(p->as) {
	case ATEXT:
	case ADATA:
	case AGLOBL:
		if(p->from.sym == S)
			mangle(pn);
		break;
	}

	if(debug['W'])
		print("%P\n", p);

	switch(p->as) {
	case AHISTORY:
		if(p->to.offset == -1) {
			addlib(src, pn);
			histfrogp = 0;
			goto loop;
		}
		if(src[0] == '\0')
			copyhistfrog(src, sizeof src);
		addhist(p->line, D_FILE);		/* 'z' */
		if(p->to.offset)
			addhist(p->to.offset, D_FILE1);	/* 'Z' */
		histfrogp = 0;
		goto loop;

	case AEND:
		histtoauto();
		if(cursym != nil && cursym->text)
			cursym->autom = curauto;
		curauto = 0;
		cursym = nil;
		if(Boffset(f) == eof)
			return;
		goto newloop;

	case AGLOBL:
		s = p->from.sym;
		if(s->type == 0 || s->type == SXREF) {
			s->type = SBSS;
			s->size = 0;
		}
		if(s->type != SBSS && !s->dupok) {
			diag("%s: redefinition: %s in %s",
				pn, s->name, TNAME);
			s->type = SBSS;
			s->size = 0;
		}
		if(p->to.offset > s->size)
			s->size = p->to.offset;
		if(p->from.scale & DUPOK)
			s->dupok = 1;
		if(p->from.scale & RODATA)
			s->type = SRODATA;
		goto loop;

	case ADATA:
		// Assume that AGLOBL comes after ADATA.
		// If we've seen an AGLOBL that said this sym was DUPOK,
		// ignore any more ADATA we see, which must be
		// redefinitions.
		s = p->from.sym;
		if(s->dupok) {
//			if(debug['v'])
//				Bprint(&bso, "skipping %s in %s: dupok\n", s->name, pn);
			goto loop;
		}
		if(s->file == nil)
			s->file = pn;
		else if(s->file != pn) {
			diag("multiple initialization for %s: in both %s and %s", s->name, s->file, pn);
			errorexit();
		}
		savedata(s, p);
		unmal(p, sizeof *p);
		goto loop;

	case AGOK:
		diag("%s: GOK opcode in %s", pn, TNAME);
		pc++;
		goto loop;

	case ATEXT:
		s = p->from.sym;
		if(s->text != nil) {
			diag("%s: %s: redefinition", pn, s->name);
			return;
		}
		if(ntext++ == 0 && s->type != 0 && s->type != SXREF) {
			/* redefinition, so file has probably been seen before */
			if(debug['v'])
				Bprint(&bso, "skipping: %s: redefinition: %s", pn, s->name);
			return;
		}
		if(cursym != nil && cursym->text) {
			histtoauto();
			cursym->autom = curauto;
			curauto = 0;
		}
		skip = 0;
		if(etextp)
			etextp->next = s;
		else
			textp = s;
		etextp = s;
		s->text = p;
		cursym = s;
		if(s->type != 0 && s->type != SXREF) {
			if(p->from.scale & DUPOK) {
				skip = 1;
				goto casdef;
			}
			diag("%s: redefinition: %s\n%P", pn, s->name, p);
		}
		if(fromgotype) {
			if(s->gotype && s->gotype != fromgotype)
				diag("%s: type mismatch for %s", pn, s->name);
			s->gotype = fromgotype;
		}
		s->type = STEXT;
		s->value = pc;
		lastp = p;
		p->pc = pc++;
		goto loop;

	case AMODE:
		if(p->from.type == D_CONST || p->from.type == D_INDIR+D_NONE){
			switch((int)p->from.offset){
			case 16: case 32: case 64:
				mode = p->from.offset;
				break;
			}
		}
		goto loop;

	case AFMOVF:
	case AFADDF:
	case AFSUBF:
	case AFSUBRF:
	case AFMULF:
	case AFDIVF:
	case AFDIVRF:
	case AFCOMF:
	case AFCOMFP:
	case AMOVSS:
	case AADDSS:
	case ASUBSS:
	case AMULSS:
	case ADIVSS:
	case ACOMISS:
	case AUCOMISS:
		if(skip)
			goto casdef;
		if(p->from.type == D_FCONST) {
			/* size sb 9 max */
			sprint(literal, "$%ux", ieeedtof(&p->from.ieee));
			s = lookup(literal, 0);
			if(s->type == 0) {
				s->type = SDATA;
				adduint32(s, ieeedtof(&p->from.ieee));
				s->reachable = 1;
			}
			p->from.type = D_EXTERN;
			p->from.sym = s;
			p->from.offset = 0;
		}
		goto casdef;

	case AFMOVD:
	case AFADDD:
	case AFSUBD:
	case AFSUBRD:
	case AFMULD:
	case AFDIVD:
	case AFDIVRD:
	case AFCOMD:
	case AFCOMDP:
	case AMOVSD:
	case AADDSD:
	case ASUBSD:
	case AMULSD:
	case ADIVSD:
	case ACOMISD:
	case AUCOMISD:
		if(skip)
			goto casdef;
		if(p->from.type == D_FCONST) {
			/* size sb 18 max */
			sprint(literal, "$%ux.%ux",
				p->from.ieee.l, p->from.ieee.h);
			s = lookup(literal, 0);
			if(s->type == 0) {
				s->type = SDATA;
				adduint32(s, p->from.ieee.l);
				adduint32(s, p->from.ieee.h);
				s->reachable = 1;
			}
			p->from.type = D_EXTERN;
			p->from.sym = s;
			p->from.offset = 0;
		}
		goto casdef;

	casdef:
	default:
		if(skip)
			nopout(p);
		p->pc = pc;
		pc++;

		if(p->to.type == D_BRANCH)
			p->to.offset += ipc;
		if(lastp == nil) {
			if(p->as != ANOP)
				diag("unexpected instruction: %P", p);
			goto loop;
		}
		lastp->link = p;
		lastp = p;
		goto loop;
	}
	goto loop;

eof:
	diag("truncated object file: %s", pn);
}

Prog*
prg(void)
{
	Prog *p;

	p = mal(sizeof(*p));

	*p = zprg;
	return p;
}

Prog*
copyp(Prog *q)
{
	Prog *p;

	p = prg();
	*p = *q;
	return p;
}

Prog*
appendp(Prog *q)
{
	Prog *p;

	p = prg();
	p->link = q->link;
	q->link = p;
	p->line = q->line;
	p->mode = q->mode;
	return p;
}
