// ??
static char*
linkername[] =
{
	"runtime.softfloat",
};

void
usage(void)
{
	fprint(2, "usage: ... [-L dir] [-r path]\n");
	errorexit();
}

void
main(int argc, char *argv[])
{
	int c, i;

	ARGBEGIN {
	case 'L':
		Lflag(EARGF(usage()));
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



	dtype = 4;

	version = 0;
    ...

	addlibpath("command line", "command line", argv[0], "main");

	loadlib();

	// mark some functions that are only referenced after linker code editing
	// TODO(kaib): this doesn't work, the prog can't be found in runtime
	for(i=0; i<nelem(linkername); i++)
		mark(lookup(linkername[i], 0));

	deadcode();

	if(textp == nil) {
		diag("no code");
		errorexit();
	}

	patch();

	if(debug['p'])
		if(debug['1'])
			doprof1();
		else
			doprof2();

	doelf();
	dodata();

	follow();

	softfloat();

	noops();
	span();

	reloc();

	asmb();

	undef();

	if(debug['c']){
		thumbcount();
		print("ARM size = %d\n", armsize);
	}
	errorexit();
}


void
nopout(Prog *p)
{
	p->as = ANOP;
	p->from.type = D_NONE;
	p->to.type = D_NONE;
}

static void puntfp(Prog *);

void
ldobj1(Biobuf *f, char *pkg, int64 len, char *pn)
{
	int32 ipc;
	Prog *p;
	Sym *h[NSYM], *s, *di;
	int v, o, r, skip;
	uint32 sig;
	char *name;
	int ntext;
	int32 eof;
	char src[1024], *x;
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

loop:
	if(f->state == Bracteof || Boffset(f) >= eof)
		goto eof;
	o = Bgetc(f);
	if(o == Beof)
		goto eof;

	if(o <= AXXX || o >= ALAST) {
		diag("%s:#%lld: opcode out of range: %#ux", pn, Boffset(f), o);
		print("	probably not a .5 file\n");
		errorexit();
	}
	if(o == ANAME || o == ASIGNAME) {
		sig = 0;
		if(o == ASIGNAME)
			sig = Bget4(f);
		v = Bgetc(f); /* type */
		o = Bgetc(f); /* sym */
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

		if(sig != 0){
			if(s->sig != 0 && s->sig != sig)
				diag("incompatible type signatures %ux(%s) and %ux(%s) for %s", s->sig, s->file, sig, pn, s->name);
			s->sig = sig;
			s->file = pn;
		}

		if(debug['W'])
			print("	ANAME	%s\n", s->name);
		if(o < 0 || o >= nelem(h)) {
			fprint(2, "%s: mangled input file\n", pn);
			errorexit();
		}
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
		}
		goto loop;
	}

	p = mal(sizeof(Prog));
	p->as = o;
	p->scond = Bgetc(f);
	p->reg = Bgetc(f);
	p->line = Bget4(f);

	zaddr(f, &p->from, h);
	zaddr(f, &p->to, h);

	if(p->as != ATEXT && p->as != AGLOBL && p->reg > NREG)
		diag("register out of range %A %d", p->as, p->reg);

	p->link = P;
	p->cond = P;

	if(debug['W'])
		print("%P\n", p);

	switch(o) {
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
		if(s == S) {
			diag("GLOBL must have a name\n%P", p);
			errorexit();
		}
		if(s->type == 0 || s->type == SXREF) {
			s->type = SBSS;
			s->value = 0;
		}
		if(s->type != SBSS) {
			diag("redefinition: %s\n%P", s->name, p);
			s->type = SBSS;
			s->value = 0;
		}
		if(p->to.offset > s->size)
			s->size = p->to.offset;
		if(p->reg & DUPOK)
			s->dupok = 1;
		break;

	case ADATA:
		// Assume that AGLOBL comes after ADATA.
		// If we've seen an AGLOBL that said this sym was DUPOK,
		// ignore any more ADATA we see, which must be
		// redefinitions.
		s = p->from.sym;
		if(s->dupok) {
			if(debug['v'])
				Bprint(&bso, "skipping %s in %s: dupok\n", s->name, pn);
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
		break;

    ...
	case ATEXT:
		if(cursym != nil && cursym->text) {
			histtoauto();
			cursym->autom = curauto;
			curauto = 0;
		}
		s = p->from.sym;
		if(s == S) {
			diag("TEXT must have a name\n%P", p);
			errorexit();
		}
		cursym = s;
		if(ntext++ == 0 && s->type != 0 && s->type != SXREF) {
			/* redefinition, so file has probably been seen before */
			if(debug['v'])
				Bprint(&bso, "skipping: %s: redefinition: %s", pn, s->name);
			return;
		}
		skip = 0;
		if(s->type != 0 && s->type != SXREF) {
			if(p->reg & DUPOK) {
				skip = 1;
				goto casedef;
			}
			diag("redefinition: %s\n%P", s->name, p);
		}
		if(etextp)
			etextp->next = s;
		else
			textp = s;
		etextp = s;
		setarch(p);
		setthumb(p);
		p->align = 4;
		autosize = (p->to.offset+3L) & ~3L;
		p->to.offset = autosize;
		autosize += 4;
		s->type = STEXT;
		s->text = p;
		s->value = pc;
		s->thumb = thumb;
		lastp = p;
		p->pc = pc;
		pc++;
		break;

	case ASUB:
		if(p->from.type == D_CONST)
		if(p->from.name == D_NONE)
		if(p->from.offset < 0) {
			p->from.offset = -p->from.offset;
			p->as = AADD;
		}
		goto casedef;

	case AADD:
		if(p->from.type == D_CONST)
		if(p->from.name == D_NONE)
		if(p->from.offset < 0) {
			p->from.offset = -p->from.offset;
			p->as = ASUB;
		}
		goto casedef;

	case AMOVWD:
	case AMOVWF:
	case AMOVDW:
	case AMOVFW:
	case AMOVFD:
	case AMOVDF:
	case ACMPF:
	case ACMPD:
	case AADDF:
	case AADDD:
	case ASUBF:
	case ASUBD:
	case AMULF:
	case AMULD:
	case ADIVF:
	case ADIVD:
		if(thumb)
			puntfp(p);
		goto casedef;

	case AMOVF:
		if(thumb)
			puntfp(p);
		if(skip)
			goto casedef;

		if(p->from.type == D_FCONST && chipfloat(&p->from.ieee) < 0) {
			/* size sb 9 max */
			sprint(literal, "$%ux", ieeedtof(&p->from.ieee));
			s = lookup(literal, 0);
			if(s->type == 0) {
				s->type = SBSS;
				adduint32(s, ieeedtof(&p->from.ieee));
				s->reachable = 0;
			}
			p->from.type = D_OREG;
			p->from.sym = s;
			p->from.name = D_EXTERN;
			p->from.offset = 0;
		}
		goto casedef;

	case AMOVD:
		if(thumb)
			puntfp(p);
		if(skip)
			goto casedef;

		if(p->from.type == D_FCONST && chipfloat(&p->from.ieee) < 0) {
			/* size sb 18 max */
			sprint(literal, "$%ux.%ux",
				p->from.ieee.l, p->from.ieee.h);
			s = lookup(literal, 0);
			if(s->type == 0) {
				s->type = SBSS;
				adduint32(s, p->from.ieee.l);
				adduint32(s, p->from.ieee.h);
				s->reachable = 0;
			}
			p->from.type = D_OREG;
			p->from.sym = s;
			p->from.name = D_EXTERN;
			p->from.offset = 0;
		}
		goto casedef;

	default:
	casedef:
		if(skip)
			nopout(p);
		p->pc = pc;
		pc++;
		if(p->to.type == D_BRANCH)
			p->to.offset += ipc;
		if(lastp == nil) {
			if(p->as != ANOP)
				diag("unexpected instruction: %P", p);
			break;
		}
		lastp->link = p;
		lastp = p;
		break;
	}
	goto loop;

eof:
	diag("truncated object file: %s", pn);
}

static void
puntfp(Prog *p)
{
	USED(p);
	/* floating point - punt for now */
	cursym->text->reg = NREG;	/* ARM */
	cursym->thumb = 0;
	thumb = 0;
	// print("%s: generating ARM code (contains floating point ops %d)\n", curtext->from.sym->name, p->line);
}
